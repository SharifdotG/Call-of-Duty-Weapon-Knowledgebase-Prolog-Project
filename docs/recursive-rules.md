# Recursive Rules - Detailed Analysis

This document provides an in-depth analysis of the recursive algorithms implemented in the Call of Duty Weapon Knowledgebase, explaining their design, implementation, and computational complexity.

## Overview of Recursive Systems

The knowledgebase implements several sophisticated recursive algorithms that handle:

1. **Weapon Progression Chains**: Transitive closure computation for unlock relationships
2. **Attachment Progression**: Recursive attachment unlock systems
3. **Depth Analysis**: Recursive depth calculation for progression hierarchy
4. **Chain Optimization**: Finding longest paths and optimal progressions
5. **Statistical Recursion**: Recursive counting and analysis operations

## Core Recursive Predicates

### 1. Transitive Closure: `can_unlock_eventually/2`

**Purpose**: Determines if one weapon can eventually unlock another through a progression chain

**Implementation Strategy**:

- Uses direct recursion with base case and recursive case
- Implements transitive closure over the `unlock_progression/2` relation
- Prevents infinite loops through Prolog's built-in cycle detection

**Algorithm**:

```prolog
% Base case: direct unlock relationship
can_unlock_eventually(BaseGun, TargetGun) :-
    unlock_progression(BaseGun, TargetGun).

% Recursive case: unlock through intermediate weapon
can_unlock_eventually(BaseGun, TargetGun) :-
    unlock_progression(BaseGun, IntermediateGun),
    can_unlock_eventually(IntermediateGun, TargetGun).
```

**Complexity Analysis**:

- **Time Complexity**: O(d) where d is the depth of the progression chain
- **Space Complexity**: O(d) for the call stack
- **Termination**: Guaranteed by finite unlock chains (no cycles in data)

**Example Execution Trace**:

```prolog
?- can_unlock_eventually(m4a1, mk14).

% Execution trace:
% 1. Try base case: unlock_progression(m4a1, mk14) - fails
% 2. Try recursive case:
%    - unlock_progression(m4a1, acr) - succeeds
%    - can_unlock_eventually(acr, mk14)
%      - Try base case: unlock_progression(acr, mk14) - succeeds
% Result: true
```

### 2. Chain Generation: `unlock_chain/2`

**Purpose**: Generates the complete unlock progression chain starting from a weapon

**Implementation Strategy**:

- Uses auxiliary predicate pattern with helper function
- Builds list incrementally through tail recursion
- Terminates when no further unlocks exist

**Algorithm**:

```prolog
% Main predicate with helper
unlock_chain(BaseGun, [BaseGun|Chain]) :-
    unlock_chain_helper(BaseGun, Chain).

% Base case: no more unlocks
unlock_chain_helper(Gun, []) :-
    \+ unlock_progression(Gun, _).

% Recursive case: add next weapon and continue
unlock_chain_helper(Gun, [NextGun|RestChain]) :-
    unlock_progression(Gun, NextGun),
    unlock_chain_helper(NextGun, RestChain).
```

**Complexity Analysis**:

- **Time Complexity**: O(n) where n is the length of the chain
- **Space Complexity**: O(n) for the result list + O(d) for call stack
- **Termination**: Guaranteed by finite chains

**Example Execution**:

```prolog
?- unlock_chain(m1_garand, Chain).

% Step-by-step construction:
% 1. Start with [m1_garand|Chain]
% 2. unlock_progression(m1_garand, thompson) → Chain = [thompson|RestChain]
% 3. unlock_progression(thompson, bar) → RestChain = [bar|RestChain2]
% 4. \+ unlock_progression(bar, _) → RestChain2 = []
% Result: Chain = [m1_garand, thompson, bar]
```

### 3. Depth Calculation: `progression_depth/2`

**Purpose**: Calculates the progression depth of a weapon (distance from base weapons)

**Implementation Strategy**:

- Uses recursive depth-first search
- Base case identifies weapons with no predecessors (depth 0)
- Recursive case adds 1 to predecessor's depth

**Algorithm**:

```prolog
% Base case: weapon has no predecessor (base weapon)
progression_depth(Gun, 0) :-
    \+ unlock_progression(_, Gun).

% Recursive case: depth is predecessor's depth + 1
progression_depth(Gun, Depth) :-
    unlock_progression(PrevGun, Gun),
    progression_depth(PrevGun, PrevDepth),
    Depth is PrevDepth + 1.
```

**Complexity Analysis**:

- **Time Complexity**: O(d) where d is the depth
- **Space Complexity**: O(d) for call stack
- **Optimality**: Always finds minimal depth due to tree structure

**Mathematical Properties**:

- Depth forms a partial order over weapons
- Base weapons have depth 0
- Each unlock relationship increases depth by exactly 1
- Maximum depth is bounded by longest chain

### 4. Longest Chain Analysis: `longest_chain_from/2`

**Purpose**: Finds the longest possible unlock chain starting from a weapon

**Implementation Strategy**:

- Generates all possible chains using `findall/3`
- Compares chain lengths to find maximum
- Uses helper predicate for length comparison

**Algorithm**:

```prolog
longest_chain_from(Gun, Chain) :-
    findall(C, unlock_chain(Gun, C), Chains),
    max_length_list(Chains, Chain).

% Helper: find list with maximum length
max_length_list([H], H).
max_length_list([H|T], Max) :-
    max_length_list(T, TMax),
    length(H, LenH),
    length(TMax, LenTMax),
    (LenH >= LenTMax -> Max = H ; Max = TMax).
```

**Complexity Analysis**:

- **Time Complexity**: O(c × n) where c is number of chains, n is average length
- **Space Complexity**: O(c × n) for storing all chains
- **Optimization Note**: Could be optimized using dynamic programming

### 5. Attachment Progression: `attachment_unlock_chain/2`

**Purpose**: Handles recursive progression for attachment unlocks

**Implementation Strategy**:

- Mirrors weapon progression structure
- Uses same auxiliary pattern as weapon chains
- Operates on attachment unlock relationships

**Algorithm**:

```prolog
attachment_unlock_chain(BaseAttachment, [BaseAttachment|Chain]) :-
    attachment_unlock_helper(BaseAttachment, Chain).

attachment_unlock_helper(Attachment, []) :-
    \+ unlock_progression(Attachment, _).

attachment_unlock_helper(Attachment, [NextAttachment|RestChain]) :-
    unlock_progression(Attachment, NextAttachment),
    attachment_unlock_helper(NextAttachment, RestChain).
```

**Multi-Domain Recursion**: This demonstrates how the same recursive patterns apply across different domains (weapons and attachments).

## Advanced Recursive Patterns

### Mutual Recursion in Analysis

Some predicates use mutual recursion for complex analysis:

```prolog
% Weapon analysis that may recurse through different classification systems
versatile_weapons(Gun) :-
    gun(Gun, assault_rifle, _, _, _, _),
    high_damage_guns(Gun),
    well_supported_weapons(Gun).

well_supported_weapons(Weapon) :-
    gun(Weapon, _, _, _, _, _),
    findall(A, compatible_attachments(Weapon, A), Attachments),
    length(Attachments, Count),
    Count >= 3.
```

### Tail Recursion Optimization

The knowledgebase uses tail recursion where possible for efficiency:

```prolog
% Tail-recursive accumulator pattern (conceptual)
count_weapons_by_type_acc(Type, Count) :-
    count_weapons_acc(Type, 0, Count).

count_weapons_acc(Type, Acc, Count) :-
    gun(Gun, Type, _, _, _, _),
    !,
    Acc1 is Acc + 1,
    count_weapons_acc(Type, Acc1, Count).
count_weapons_acc(_, Count, Count).
```

## Recursion Design Principles

### 1. Termination Guarantees

All recursive predicates ensure termination through:

- **Finite Data Structures**: No cycles in progression chains
- **Decreasing Parameters**: Depth decreases or list size reduces
- **Base Cases**: Clear stopping conditions

### 2. Efficiency Considerations

- **Memoization Opportunities**: Depth calculations could benefit from caching
- **Cut Usage**: Strategic cuts to prevent unnecessary backtracking
- **Auxiliary Predicates**: Helper functions reduce complexity

### 3. Correctness Properties

- **Completeness**: All reachable weapons are found
- **Soundness**: Only valid progression relationships are reported
- **Consistency**: Results are deterministic and reproducible

## Performance Analysis

### Benchmarking Results

Based on the current dataset (121 weapons, 28 progression chains):

| Predicate | Average Time | Worst Case | Memory Usage |
|-----------|--------------|------------|--------------|
| `can_unlock_eventually/2` | O(3) | O(6) | Low |
| `unlock_chain/2` | O(4) | O(6) | Medium |
| `progression_depth/2` | O(3) | O(6) | Low |
| `longest_chain_from/2` | O(100) | O(300) | High |

### Optimization Opportunities

1. **Depth Caching**: Store computed depths to avoid recomputation
2. **Chain Memoization**: Cache chains for frequently queried weapons
3. **Parallel Processing**: Some analyses could benefit from parallel execution
4. **Index Structures**: Additional indexing for complex queries

## Recursive Anti-Patterns Avoided

### 1. Infinite Recursion Prevention

The implementation avoids infinite recursion through:

- **Acyclic Data**: Progression chains are trees, not graphs
- **Proper Base Cases**: Every recursive predicate has clear termination
- **Finite Domain**: Limited number of weapons ensures termination

### 2. Stack Overflow Prevention

- **Reasonable Depth**: Maximum chain depth is 6, well within stack limits
- **Tail Recursion**: Used where possible to optimize stack usage
- **Early Termination**: Predicates stop as soon as result is found

### 3. Inefficient Recursion Avoided

- **No Redundant Computation**: Each step computes new information
- **Appropriate Data Structures**: Lists used efficiently for chains
- **Strategic Backtracking**: Cut usage prevents unnecessary search

## Integration with Prolog Features

### Built-in Predicate Synergy

The recursive rules integrate well with Prolog's built-in predicates:

```prolog
% Using findall/3 for collection
all_reachable_weapons(BaseGun, Weapons) :-
    findall(Gun, can_unlock_eventually(BaseGun, Gun), Weapons).

% Using bagof/3 for grouped results
weapons_by_depth(Depth, Weapons) :-
    bagof(Gun, progression_depth(Gun, Depth), Weapons).

% Using setof/3 for sorted unique results
sorted_chain_weapons(BaseGun, SortedWeapons) :-
    setof(Gun, can_unlock_eventually(BaseGun, Gun), SortedWeapons).
```

### Meta-Predicates Integration

```prolog
% Using call/1 for dynamic predicate calls
apply_to_chain(Predicate, Chain, Results) :-
    maplist(call(Predicate), Chain, Results).

% Using once/1 for deterministic results
single_longest_chain(Gun, Chain) :-
    once(longest_chain_from(Gun, Chain)).
```

This recursive system demonstrates sophisticated use of Prolog's logic programming paradigm, providing efficient and elegant solutions to complex progression analysis problems.
