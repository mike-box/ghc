{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Cmm.Sink (
     cmmSink
  ) where

import GHC.Prelude

import GHC.Cmm
import GHC.Cmm.Alias
import GHC.Cmm.Opt
import GHC.Cmm.Liveness
import GHC.Cmm.LRegSet
import GHC.Cmm.Utils
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Platform.Regs

import GHC.Platform
import GHC.Types.Unique.FM

import qualified Data.IntSet as IntSet
import Data.List (partition)
import Data.Maybe

import GHC.Exts (inline)

import GHC.Utils.Outputable

-- -----------------------------------------------------------------------------
-- Sinking and inlining

-- This is an optimisation pass that
--  (a) moves assignments closer to their uses, to reduce register pressure
--  (b) pushes assignments into a single branch of a conditional if possible
--  (c) inlines assignments to registers that are mentioned only once
--  (d) discards dead assignments
--
-- This tightens up lots of register-heavy code.  It is particularly
-- helpful in the Cmm generated by the Stg->Cmm code generator, in
-- which every function starts with a copyIn sequence like:
--
--    x1 = R1
--    x2 = Sp[8]
--    x3 = Sp[16]
--    if (Sp - 32 < SpLim) then L1 else L2
--
-- we really want to push the x1..x3 assignments into the L2 branch.
--
-- Algorithm:
--
--  * Start by doing liveness analysis.
--
--  * Keep a list of assignments A; earlier ones may refer to later ones.
--    Currently we only sink assignments to local registers, because we don't
--    have liveness information about global registers.
--
--  * Walk forwards through the graph, look at each node N:
--
--    * If it is a dead assignment, i.e. assignment to a register that is
--      not used after N, discard it.
--
--    * Try to inline based on current list of assignments
--      * If any assignments in A (1) occur only once in N, and (2) are
--        not live after N, inline the assignment and remove it
--        from A.
--
--      * If an assignment in A is cheap (RHS is local register), then
--        inline the assignment and keep it in A in case it is used afterwards.
--
--      * Otherwise don't inline.
--
--    * If N is assignment to a local register pick up the assignment
--      and add it to A.
--
--    * If N is not an assignment to a local register:
--      * remove any assignments from A that conflict with N, and
--        place them before N in the current block.  We call this
--        "dropping" the assignments.
--
--      * An assignment conflicts with N if it:
--        - assigns to a register mentioned in N
--        - mentions a register assigned by N
--        - reads from memory written by N
--      * do this recursively, dropping dependent assignments
--
--    * At an exit node:
--      * drop any assignments that are live on more than one successor
--        and are not trivial
--      * if any successor has more than one predecessor (a join-point),
--        drop everything live in that successor. Since we only propagate
--        assignments that are not dead at the successor, we will therefore
--        eliminate all assignments dead at this point. Thus analysis of a
--        join-point will always begin with an empty list of assignments.
--
--
-- As a result of above algorithm, sinking deletes some dead assignments
-- (transitively, even).  This isn't as good as removeDeadAssignments,
-- but it's much cheaper.

-- -----------------------------------------------------------------------------
-- things that we aren't optimising very well yet.
--
-- -----------
-- (1) From GHC's FastString.hashStr:
--
--  s2ay:
--      if ((_s2an::I64 == _s2ao::I64) >= 1) goto c2gn; else goto c2gp;
--  c2gn:
--      R1 = _s2au::I64;
--      call (I64[Sp])(R1) args: 8, res: 0, upd: 8;
--  c2gp:
--      _s2cO::I64 = %MO_S_Rem_W64(%MO_UU_Conv_W8_W64(I8[_s2aq::I64 + (_s2an::I64 << 0)]) + _s2au::I64 * 128,
--                                 4091);
--      _s2an::I64 = _s2an::I64 + 1;
--      _s2au::I64 = _s2cO::I64;
--      goto s2ay;
--
-- a nice loop, but we didn't eliminate the silly assignment at the end.
-- See Note [dependent assignments], which would probably fix this.
-- This is #8336.
--
-- -----------
-- (2) From stg_atomically_frame in PrimOps.cmm
--
-- We have a diamond control flow:
--
--     x = ...
--       |
--      / \
--     A   B
--      \ /
--       |
--    use of x
--
-- Now x won't be sunk down to its use, because we won't push it into
-- both branches of the conditional.  We certainly do have to check
-- that we can sink it past all the code in both A and B, but having
-- discovered that, we could sink it to its use.
--

-- -----------------------------------------------------------------------------

type Assignment = (LocalReg, CmmExpr, AbsMem)
  -- Assignment caches AbsMem, an abstraction of the memory read by
  -- the RHS of the assignment.

type Assignments = [Assignment]
  -- A sequence of assignments; kept in *reverse* order
  -- So the list [ x=e1, y=e2 ] means the sequence of assignments
  --     y = e2
  --     x = e1

cmmSink :: Platform -> CmmGraph -> CmmGraph
cmmSink platform graph = ofBlockList (g_entry graph) $ sink mapEmpty $ blocks
  where
  liveness = cmmLocalLivenessL platform graph
  getLive l = mapFindWithDefault emptyLRegSet l liveness

  blocks = revPostorder graph

  join_pts = findJoinPoints blocks :: LabelMap Int -- Block -> Number of Predecessors

  sink :: LabelMap Assignments -> [CmmBlock] -> [CmmBlock]
  sink _ [] = []
  sink sunk (b:bs) =
    -- pprTrace "sink" (ppr lbl) $
    blockJoin first final_middle final_last : sink sunk' bs
    where
      lbl = entryLabel b
      (first, middle, last) = blockSplit b

      succs = successors last

      -- Annotate the middle nodes with the registers live *after*
      -- the node.  This will help us decide whether we can inline
      -- an assignment in the current node or not.
      live = IntSet.unions (map getLive succs)
      live_middle = gen_killL platform last live
      ann_middles = annotate platform live_middle (blockToList middle)

      -- Now sink and inline in this block
      (middle', assigs) = walk platform ann_middles (mapFindWithDefault [] lbl sunk)
      fold_last = constantFoldNode platform last
      (final_last, assigs') = tryToInline platform live fold_last assigs

      -- We cannot sink into join points (successors with more than
      -- one predecessor), so identify the join points and the set
      -- of registers live in them.
      (joins, nonjoins) = partition (`mapMember` join_pts) succs
      live_in_joins = IntSet.unions (map getLive joins)

      -- We do not want to sink an assignment into multiple branches,
      -- so identify the set of registers live in multiple successors.
      -- This is made more complicated because when we sink an assignment
      -- into one branch, this might change the set of registers that are
      -- now live in multiple branches.
      init_live_sets = map getLive nonjoins
      live_in_multi live_sets r =
         case filter (elemLRegSet r) live_sets of
           (_one:_two:_) -> True
           _ -> False

      -- Now, drop any assignments that we will not sink any further.
      (dropped_last, assigs'') = dropAssignments platform drop_if init_live_sets assigs'

      drop_if :: (LocalReg, CmmExpr, AbsMem)
                      -> [LRegSet] -> (Bool, [LRegSet])
      drop_if a@(r,rhs,_) live_sets = (should_drop, live_sets')
          where
            should_drop =  conflicts platform a final_last
                        || not (isTrivial platform rhs) && live_in_multi live_sets r
                        || r `elemLRegSet` live_in_joins

            live_sets' | should_drop = live_sets
                       | otherwise   = map upd live_sets

            upd set | r `elemLRegSet` set = set `IntSet.union` live_rhs
                    | otherwise          = set

            live_rhs = foldRegsUsed platform (flip insertLRegSet) emptyLRegSet rhs

      final_middle = foldl' blockSnoc middle' dropped_last

      sunk' = mapUnion sunk $
                 mapFromList [ (l, filterAssignments platform (getLive l) assigs'')
                             | l <- succs ]

{- TODO: enable this later, when we have some good tests in place to
   measure the effect and tune it.

-- small: an expression we don't mind duplicating
isSmall :: CmmExpr -> Bool
isSmall (CmmReg (CmmLocal _)) = True  --
isSmall (CmmLit _) = True
isSmall (CmmMachOp (MO_Add _) [x,y]) = isTrivial x && isTrivial y
isSmall (CmmRegOff (CmmLocal _) _) = True
isSmall _ = False
-}

--
-- We allow duplication of trivial expressions: registers (both local and
-- global) and literals.
--
isTrivial :: Platform -> CmmExpr -> Bool
isTrivial _ (CmmReg (CmmLocal _)) = True
isTrivial platform (CmmReg (CmmGlobal r)) = -- see Note [Inline GlobalRegs?]
  if isARM (platformArch platform)
  then True -- CodeGen.Platform.ARM does not have globalRegMaybe
  else isJust (globalRegMaybe platform r)
  -- GlobalRegs that are loads from BaseReg are not trivial
isTrivial _ (CmmLit _) = True
isTrivial _ _          = False

--
-- annotate each node with the set of registers live *after* the node
--
annotate :: Platform -> LRegSet -> [CmmNode O O] -> [(LRegSet, CmmNode O O)]
annotate platform live nodes = snd $ foldr ann (live,[]) nodes
  where ann n (live,nodes) = (gen_killL platform n live, (live,n) : nodes)

--
-- Find the blocks that have multiple successors (join points)
--
findJoinPoints :: [CmmBlock] -> LabelMap Int
findJoinPoints blocks = mapFilter (>1) succ_counts
 where
  all_succs = concatMap successors blocks :: [Label]

  succ_counts :: LabelMap Int
  succ_counts = foldl' (\m l -> mapInsertWith (+) l 1 m) mapEmpty all_succs

--
-- filter the list of assignments to remove any assignments that
-- are not live in a continuation.
--
filterAssignments :: Platform -> LRegSet -> Assignments -> Assignments
filterAssignments platform live assigs = reverse (go assigs [])
  where go []             kept = kept
        go (a@(r,_,_):as) kept | needed    = go as (a:kept)
                               | otherwise = go as kept
           where
              needed = r `elemLRegSet` live
                       || any (conflicts platform a) (map toNode kept)
                       --  Note that we must keep assignments that are
                       -- referred to by other assignments we have
                       -- already kept.

-- -----------------------------------------------------------------------------
-- Walk through the nodes of a block, sinking and inlining assignments
-- as we go.
--
-- On input we pass in a:
--    * list of nodes in the block
--    * a list of assignments that appeared *before* this block and
--      that are being sunk.
--
-- On output we get:
--    * a new block
--    * a list of assignments that will be placed *after* that block.
--

walk :: Platform
     -> [(LRegSet, CmmNode O O)]    -- nodes of the block, annotated with
                                        -- the set of registers live *after*
                                        -- this node.

     -> Assignments                     -- The current list of
                                        -- assignments we are sinking.
                                        -- Earlier assignments may refer
                                        -- to later ones.

     -> ( Block CmmNode O O             -- The new block
        , Assignments                   -- Assignments to sink further
        )

walk platform nodes assigs = go nodes emptyBlock assigs
 where
   go []               block as = (block, as)
   go ((live,node):ns) block as
    -- discard nodes representing dead assignment
    | shouldDiscard node live             = go ns block as
    -- sometimes only after simplification we can tell we can discard the node.
    -- See Note [Discard simplified nodes]
    | noOpAssignment node2                = go ns block as
    -- Pick up interesting assignments
    | Just a <- shouldSink platform node2 = go ns block (a : as1)
    -- Try inlining, drop assignments and move on
    | otherwise                           = go ns block' as'
    where
      -- Simplify node
      node1 = constantFoldNode platform node

      -- Inline assignments
      (node2, as1) = tryToInline platform live node1 as

      -- Drop any earlier assignments conflicting with node2
      (dropped, as') = dropAssignmentsSimple platform
                          (\a -> conflicts platform a node2) as1

      -- Walk over the rest of the block. Includes dropped assignments
      block' = foldl' blockSnoc block dropped `blockSnoc` node2

{- Note [Discard simplified nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a sequence like this:

      _c1::P64 = R1;
      _c3::I64 = I64[_c1::P64 + 1];
      R1 = _c1::P64;
      P64[Sp - 72] = _c1::P64;
      I64[Sp - 64] = _c3::I64;

If we discard assignments *before* simplifying nodes when we get to `R1 = _c1`.
This is then simplified into `R1 = `R1` and as a consequence prevents sinking of
loads from R1. What happens is that we:
    * Check if we can discard the node `R1 = _c1 (no)
    * Simplify the node to R1 = R1
    * We check all remaining assignments for conflicts.
    * The assignment `_c3 = [R1 + 1]`; (R1 already inlined on pickup)
      conflicts with R1 = R1, because it reads `R1` and the node writes
      to R1
    * This is clearly no-sensical because `R1 = R1` doesn't affect R1's value.

The solutions is to check if we can discard nodes before and *after* simplifying
them. We could only do it after as well, but I assume doing it early might save
some work.

That is if we process a assignment node we now:
    * Check if it can be discarded (because it's dead or a no-op)
    * Simplify the rhs of the assignment.
    * New: Check again if it might be a no-op now.
    * ...

This can help with problems like the one reported in #20334. For a full example see the test
cmm_sink_sp.

-}

--
-- Heuristic to decide whether to pick up and sink an assignment
-- Currently we pick up all assignments to local registers.  It might
-- be profitable to sink assignments to global regs too, but the
-- liveness analysis doesn't track those (yet) so we can't.
--
shouldSink :: Platform -> CmmNode e x -> Maybe Assignment
shouldSink platform (CmmAssign (CmmLocal r) e) | no_local_regs = Just (r, e, exprMem platform e)
  where no_local_regs = True -- foldRegsUsed (\_ _ -> False) True e
shouldSink _ _other = Nothing

--
-- discard dead assignments.  This doesn't do as good a job as
-- removeDeadAssignments, because it would need multiple passes
-- to get all the dead code, but it catches the common case of
-- superfluous reloads from the stack that the stack allocator
-- leaves behind.
--
-- Also we catch "r = r" here.  You might think it would fall
-- out of inlining, but the inliner will see that r is live
-- after the instruction and choose not to inline r in the rhs.
--
shouldDiscard :: CmmNode e x -> LRegSet -> Bool
shouldDiscard node live
   = case node of
       -- r = r
       CmmAssign r (CmmReg r') | r == r' -> True
       -- r = e, r is dead after assignment
       CmmAssign (CmmLocal r) _ -> not (r `elemLRegSet` live)
       _otherwise -> False

noOpAssignment :: CmmNode e x -> Bool
noOpAssignment node
   = case node of
       -- r = r
       CmmAssign r (CmmReg r') | r == r' -> True
       _otherwise -> False


toNode :: Assignment -> CmmNode O O
toNode (r,rhs,_) = CmmAssign (CmmLocal r) rhs

dropAssignmentsSimple :: Platform -> (Assignment -> Bool) -> Assignments
                      -> ([CmmNode O O], Assignments)
dropAssignmentsSimple platform f = dropAssignments platform (\a _ -> (f a, ())) ()

dropAssignments :: Platform -> (Assignment -> s -> (Bool, s)) -> s -> Assignments
                -> ([CmmNode O O], Assignments)
dropAssignments platform should_drop state assigs
 = (dropped, reverse kept)
 where
   (dropped,kept) = go state assigs [] []

   go _ []             dropped kept = (dropped, kept)
   go state (assig : rest) dropped kept
      | conflict  =
          let !node = toNode assig
          in  go state' rest (node : dropped) kept
      | otherwise = go state' rest dropped (assig:kept)
      where
        (dropit, state') = should_drop assig state
        conflict = dropit || any (conflicts platform assig) dropped


-- -----------------------------------------------------------------------------
-- Try to inline assignments into a node.
-- This also does constant folding for primops, since
-- inlining opens up opportunities for doing so.

tryToInline
   :: forall x. Platform
   -> LRegSet               -- set of registers live after this
                                -- node.  We cannot inline anything
                                -- that is live after the node, unless
                                -- it is small enough to duplicate.
   -> CmmNode O x               -- The node to inline into
   -> Assignments               -- Assignments to inline
   -> (
        CmmNode O x             -- New node
      , Assignments             -- Remaining assignments
      )

tryToInline platform liveAfter node assigs =
  -- pprTrace "tryToInline assig length:" (ppr $ length assigs) $
    let (n,as) = go usages liveAfter node emptyLRegSet assigs
    in -- pprTrace "inlined:" (pdoc platform n $$ ppr as)
       (n,as)
 where
  usages :: UniqFM LocalReg Int -- Maps each LocalReg to a count of how often it is used
  usages = foldLocalRegsUsed platform addUsage emptyUFM node

  go :: UniqFM LocalReg Int -> LRegSet -> CmmNode O x -> LRegSet -> Assignments
     -> (CmmNode O x, Assignments)
  go _usages _live node _skipped [] = (node, [])

  go usages live node skipped (a@(l,rhs,_) : rest)
   | cannot_inline            = dont_inline
   | occurs_none              = discard  -- See Note [discard during inlining]
   | occurs_once              = inline_and_discard
   | isTrivial platform rhs   = inline_and_keep
   | otherwise                = dont_inline
   where
        inline_and_discard = go usages' live inl_node skipped rest
          where usages' = foldLocalRegsUsed platform addUsage usages rhs

        discard = go usages live node skipped rest

        dont_inline        = keep node  -- don't inline the assignment, keep it
        inline_and_keep    = keep inl_node -- inline the assignment, keep it

        keep :: CmmNode O x -> (CmmNode O x, Assignments)
        keep node' = (final_node, a : rest')
          where (final_node, rest') = go usages live' node' (insertLRegSet l skipped) rest

                -- Avoid discarding of assignments to vars on the rhs.
                -- See Note [Keeping assignemnts mentioned in skipped RHSs]
                -- usages' = foldLocalRegsUsed platform (\m r -> addToUFM m r 2)
                                            -- usages rhs
                live' = inline foldLocalRegsUsed platform (\m r -> insertLRegSet r m)
                                            live rhs

        cannot_inline = skipped `regsUsedIn` rhs -- See Note [dependent assignments]
                        || l `elemLRegSet` skipped
                        || not (okToInline platform rhs node)

        -- How often is l used in the current node.
        l_usages = lookupUFM usages l
        l_live   = l `elemLRegSet` live

        occurs_once = not l_live && l_usages == Just 1
        occurs_none = not l_live && l_usages == Nothing

        inl_node = improveConditional (mapExpDeep inl_exp node)

        inl_exp :: CmmExpr -> CmmExpr
        -- inl_exp is where the inlining actually takes place!
        inl_exp (CmmReg    (CmmLocal l'))     | l == l' = rhs
        inl_exp (CmmRegOff (CmmLocal l') off) | l == l'
                    = cmmOffset platform rhs off
                    -- re-constant fold after inlining
        inl_exp (CmmMachOp op args) = cmmMachOpFold platform op args
        inl_exp other = other

{- Note [Keeping assignemnts mentioned in skipped RHSs]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    If we have to assignments: [z = y, y = e1] and we skip
    z we *must* retain the assignment y = e1. This is because
    we might inline "z = y" into another node later on so we
    must ensure y is still defined at this point.

    If we dropped the assignment of "y = e1" then we would end up
    referencing a variable which hasn't been mentioned after
    inlining.

    We use a hack to do this.

    We pretend the regs from the rhs are live after the current
    node. Since we only discard assignments to variables
    which are dead after the current block this prevents discarding of the
    assignment. It still allows inlining should e1 be a trivial rhs
    however.

-}

{- Note [improveConditional]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
cmmMachOpFold tries to simplify conditionals to turn things like
  (a == b) != 1
into
  (a != b)
but there's one case it can't handle: when the comparison is over
floating-point values, we can't invert it, because floating-point
comparisons aren't invertible (because of NaNs).

But we *can* optimise this conditional by swapping the true and false
branches. Given
  CmmCondBranch ((a >## b) != 1) t f
we can turn it into
  CmmCondBranch (a >## b) f t

So here we catch conditionals that weren't optimised by cmmMachOpFold,
and apply above transformation to eliminate the comparison against 1.

It's tempting to just turn every != into == and then let cmmMachOpFold
do its thing, but that risks changing a nice fall-through conditional
into one that requires two jumps. (see swapcond_last in
GHC.Cmm.ContFlowOpt), so instead we carefully look for just the cases where
we can eliminate a comparison.
-}
improveConditional :: CmmNode O x -> CmmNode O x
improveConditional
  (CmmCondBranch (CmmMachOp mop [x, CmmLit (CmmInt 1 _)]) t f l)
  | neLike mop, isComparisonExpr x
  = CmmCondBranch x f t (fmap not l)
  where
    neLike (MO_Ne _) = True
    neLike (MO_U_Lt _) = True   -- (x<y) < 1 behaves like (x<y) != 1
    neLike (MO_S_Lt _) = True   -- (x<y) < 1 behaves like (x<y) != 1
    neLike _ = False
improveConditional other = other

-- Note [dependent assignments]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If our assignment list looks like
--
--    [ y = e,  x = ... y ... ]
--
-- We cannot inline x.  Remember this list is really in reverse order,
-- so it means  x = ... y ...; y = e
--
-- Hence if we inline x, the outer assignment to y will capture the
-- reference in x's right hand side.
--
-- In this case we should rename the y in x's right-hand side,
-- i.e. change the list to [ y = e, x = ... y1 ..., y1 = y ]
-- Now we can go ahead and inline x.
--
-- For now we do nothing, because this would require putting
-- everything inside UniqSM.
--
-- One more variant of this (#7366):
--
--   [ y = e, y = z ]
--
-- If we don't want to inline y = e, because y is used many times, we
-- might still be tempted to inline y = z (because we always inline
-- trivial rhs's).  But of course we can't, because y is equal to e,
-- not z.

-- Note [discard during inlining]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Opportunities to discard assignments sometimes appear after we've
-- done some inlining.  Here's an example:
--
--      x = R1;
--      y = P64[x + 7];
--      z = P64[x + 15];
--      /* z is dead */
--      R1 = y & (-8);
--
-- The x assignment is trivial, so we inline it in the RHS of y, and
-- keep both x and y.  z gets dropped because it is dead, then we
-- inline y, and we have a dead assignment to x.  If we don't notice
-- that x is dead in tryToInline, we end up retaining it.

addUsage :: UniqFM LocalReg Int -> LocalReg -> UniqFM LocalReg Int
addUsage m r = addToUFM_C (+) m r 1

regsUsedIn :: LRegSet -> CmmExpr -> Bool
regsUsedIn ls _ | nullLRegSet ls = False
regsUsedIn ls e = go ls e False
  where use :: LRegSet -> CmmExpr -> Bool -> Bool
        use ls (CmmReg (CmmLocal l))      _ | l `elemLRegSet` ls = True
        use ls (CmmRegOff (CmmLocal l) _) _ | l `elemLRegSet` ls = True
        use _ls _ z = z

        go :: LRegSet -> CmmExpr -> Bool -> Bool
        go ls (CmmMachOp _ es)   z = foldr (go ls) z es
        go ls (CmmLoad addr _ _) z = go ls addr z
        go ls e                  z = use ls e z

-- we don't inline into CmmUnsafeForeignCall if the expression refers
-- to global registers.  This is a HACK to avoid global registers
-- clashing with C argument-passing registers, really the back-end
-- ought to be able to handle it properly, but currently neither PprC
-- nor the NCG can do it.  See Note [Register parameter passing]
-- See also GHC.StgToCmm.Foreign.load_args_into_temps.
okToInline :: Platform -> CmmExpr -> CmmNode e x -> Bool
okToInline platform expr node@(CmmUnsafeForeignCall{}) =
    not (globalRegistersConflict platform expr node)
okToInline _ _ _ = True

-- -----------------------------------------------------------------------------

-- | @conflicts (r,e) node@ is @False@ if and only if the assignment
-- @r = e@ can be safely commuted past statement @node@.
conflicts :: Platform -> Assignment -> CmmNode O x -> Bool
conflicts platform (r, rhs, addr) node

  -- (1) node defines registers used by rhs of assignment. This catches
  -- assignments and all three kinds of calls. See Note [Sinking and calls]
  | globalRegistersConflict platform rhs node                       = traceConflicts "conflicts1" (ppr r) True
  | localRegistersConflict  platform rhs node                       = traceConflicts "conflicts2" (ppr r) True

  -- (2) node uses register defined by assignment
  | foldRegsUsed platform (\b r' -> r == r' || b) False node        = traceConflicts "conflicts3" (ppr r) True

  -- (3) a store to an address conflicts with a read of the same memory
  | CmmStore addr' e _a <- node
  , memConflicts addr (storeAddr platform addr' (cmmExprWidth platform e))
  = traceConflicts "conflicts4" (ppr r <+> ppr addr <+> ppr (storeAddr platform addr' (cmmExprWidth platform e)) $$
                                 pdoc platform node)
    True

  -- (4) an assignment to Hp/Sp conflicts with a heap/stack read respectively
  | HeapMem{}  <- addr, CmmAssign (CmmGlobal Hp) _ <- node        = traceConflicts "conflicts6.1" (ppr r) True
  | StackMem   <- addr, CmmAssign (CmmGlobal Sp) _ <- node        = traceConflicts "conflicts6" (ppr r) True
  | SpMem{}    <- addr, CmmAssign (CmmGlobal Sp) _ <- node        = traceConflicts "conflicts7" (ppr r) True

  -- (5) foreign calls clobber heap: see Note [Foreign calls clobber heap]
  | CmmUnsafeForeignCall{} <- node, memConflicts addr AnyMem      = traceConflicts "conflicts8" (ppr r) True

  -- (6) suspendThread clobbers every global register not backed by a real
  -- register. It also clobbers heap and stack but this is handled by (5)
  | CmmUnsafeForeignCall (PrimTarget MO_SuspendThread) _ _ <- node
  , foldRegsUsed platform (\b g -> globalRegMaybe platform g == Nothing || b) False rhs
  = traceConflicts "conflicts9" (ppr r) True

  -- (7) native calls clobber any memory
  | CmmCall{} <- node, memConflicts addr AnyMem                   = traceConflicts "conflicts10" (ppr r) True

  -- (8) otherwise, no conflict
  | otherwise = traceConflicts "NoConflict" (ppr r)
                False
  where
    -- traceConflicts s d = pprTrace s d
    traceConflicts = \_s _d -> id

{- Note [Inlining foldRegsDefd]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   foldRegsDefd is, after optimization, *not* a small function so
   it's only marked INLINEABLE, but not INLINE.

   However in some specific cases we call it *very* often making it
   important to avoid the overhead of allocating the folding function.

   So we simply force inlining via the magic inline function.
   For T3294 this improves allocation with -O by ~1%.

-}

-- Returns True if node defines any global registers that are used in the
-- Cmm expression
globalRegistersConflict :: Platform -> CmmExpr -> CmmNode e x -> Bool
globalRegistersConflict platform expr node =
    -- See Note [Inlining foldRegsDefd]
    inline foldRegsDefd platform (\b r -> b || regUsedIn platform (CmmGlobal r) expr)
                 False node

-- Returns True if node defines any local registers that are used in the
-- Cmm expression
localRegistersConflict :: Platform -> CmmExpr -> CmmNode e x -> Bool
localRegistersConflict platform expr node =
    -- See Note [Inlining foldRegsDefd]
    inline foldRegsDefd platform (\b r -> b || regUsedIn platform (CmmLocal  r) expr)
                 False node

-- Note [Sinking and calls]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- We have three kinds of calls: normal (CmmCall), safe foreign (CmmForeignCall)
-- and unsafe foreign (CmmUnsafeForeignCall). We perform sinking pass after
-- stack layout (see Note [Sinking after stack layout]) which leads to two
-- invariants related to calls:
--
--   a) during stack layout phase all safe foreign calls are turned into
--      unsafe foreign calls (see Note [Lower safe foreign calls]). This
--      means that we will never encounter CmmForeignCall node when running
--      sinking after stack layout
--
--   b) stack layout saves all variables live across a call on the stack
--      just before making a call (remember we are not sinking assignments to
--      stack):
--
--       L1:
--          x = R1
--          P64[Sp - 16] = L2
--          P64[Sp - 8]  = x
--          Sp = Sp - 16
--          call f() returns L2
--       L2:
--
--      We will attempt to sink { x = R1 } but we will detect conflict with
--      { P64[Sp - 8]  = x } and hence we will drop { x = R1 } without even
--      checking whether it conflicts with { call f() }. In this way we will
--      never need to check any assignment conflicts with CmmCall. Remember
--      that we still need to check for potential memory conflicts.
--
-- So the result is that we only need to worry about CmmUnsafeForeignCall nodes
-- when checking conflicts (see Note [Unsafe foreign calls clobber caller-save registers]).
-- This assumption holds only when we do sinking after stack layout. If we run
-- it before stack layout we need to check for possible conflicts with all three
-- kinds of calls. Our `conflicts` function does that by using a generic
-- foldRegsDefd and foldRegsUsed functions defined in DefinerOfRegs and
-- UserOfRegs typeclasses.
--

{-
Note [Inline GlobalRegs?]
~~~~~~~~~~~~~~~~~~~~~~~~~

Should we freely inline GlobalRegs?

Actually it doesn't make a huge amount of difference either way, so we
*do* currently treat GlobalRegs as "trivial" and inline them
everywhere, but for what it's worth, here is what I discovered when I
(SimonM) looked into this:

Common sense says we should not inline GlobalRegs, because when we
have

  x = R1

the register allocator will coalesce this assignment, generating no
code, and simply record the fact that x is bound to $rbx (or
whatever).  Furthermore, if we were to sink this assignment, then the
range of code over which R1 is live increases, and the range of code
over which x is live decreases.  All things being equal, it is better
for x to be live than R1, because R1 is a fixed register whereas x can
live in any register.  So we should neither sink nor inline 'x = R1'.

However, not inlining GlobalRegs can have surprising
consequences. e.g. (cgrun020)

  c3EN:
      _s3DB::P64 = R1;
      _c3ES::P64 = _s3DB::P64 & 7;
      if (_c3ES::P64 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      _s3DD::P64 = P64[_s3DB::P64 + 6];
      _s3DE::P64 = P64[_s3DB::P64 + 14];
      I64[Sp - 8] = c3F0;
      R1 = _s3DE::P64;
      P64[Sp] = _s3DD::P64;

inlining the GlobalReg gives:

  c3EN:
      if (R1 & 7 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      I64[Sp - 8] = c3F0;
      _s3DD::P64 = P64[R1 + 6];
      R1 = P64[R1 + 14];
      P64[Sp] = _s3DD::P64;

but if we don't inline the GlobalReg, instead we get:

      _s3DB::P64 = R1;
      if (_s3DB::P64 & 7 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      I64[Sp - 8] = c3F0;
      R1 = P64[_s3DB::P64 + 14];
      P64[Sp] = P64[_s3DB::P64 + 6];

This looks better - we managed to inline _s3DD - but in fact it
generates an extra reg-reg move:

.Lc3EU:
        movq $c3F0_info,-8(%rbp)
        movq %rbx,%rax
        movq 14(%rbx),%rbx
        movq 6(%rax),%rax
        movq %rax,(%rbp)

because _s3DB is now live across the R1 assignment, we lost the
benefit of coalescing.

Who is at fault here?  Perhaps if we knew that _s3DB was an alias for
R1, then we would not sink a reference to _s3DB past the R1
assignment.  Or perhaps we *should* do that - we might gain by sinking
it, despite losing the coalescing opportunity.

Sometimes not inlining global registers wins by virtue of the rule
about not inlining into arguments of a foreign call, e.g. (T7163) this
is what happens when we inlined F1:

      _s3L2::F32 = F1;
      _c3O3::F32 = %MO_F_Mul_W32(F1, 10.0 :: W32);
      (_s3L7::F32) = call "ccall" arg hints:  []  result hints:  [] rintFloat(_c3O3::F32);

but if we don't inline F1:

      (_s3L7::F32) = call "ccall" arg hints:  []  result hints:  [] rintFloat(%MO_F_Mul_W32(_s3L2::F32,
                                                                                            10.0 :: W32));
-}
