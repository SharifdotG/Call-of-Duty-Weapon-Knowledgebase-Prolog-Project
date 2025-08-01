# Call of Duty Weapon Knowledgebase - Complete Documentation

This comprehensive documentation covers all aspects of the Call of Duty Weapon Knowledgebase, a sophisticated Prolog implementation modeling weapons, attachments, game mechanics, and progression systems across 21 years of Call of Duty games (2003-2024).

## Table of Contents

1. [**Quick Reference**](quick-reference.md) - Concise reference for commonly used predicates
2. [**Predicate Reference**](predicate-reference.md) - Complete listing of all 30+ predicates with syntax, parameters, and examples
3. [**Recursive Rules**](recursive-rules.md) - Detailed explanation of recursive algorithms and their implementation
4. [**Technical Implementation**](technical-implementation.md) - System architecture, data modeling, and design decisions
5. [**Usage Examples**](usage-examples.md) - Comprehensive examples with expected outputs

## Quick Navigation

### Core Systems Documentation

- **Weapon Database**: 121 weapons across 18 games with extended type system
- **Attachment System**: 49 attachments with compatibility ratings and effects
- **Game Mechanics**: 26 mechanics across progression, unlock, modification, gameplay, and economy
- **Progression System**: 28 unlock chains with recursive depth analysis

### Key Features

- **Advanced Recursive Rules**: Transitive closure, chain generation, depth calculation
- **Classification & Analysis**: Era-based, type-based, and performance-based categorization
- **Build Optimization**: Stealth, damage, and accuracy build recommendations
- **Statistical Analysis**: Weapon counting, trend analysis, and optimization

## Getting Started

```prolog
% Load the knowledgebase
?- consult('src/weapons.pl').

% Basic weapon query
?- gun_type(assault_rifle, Gun).

% Recursive progression
?- can_unlock_eventually(m4a1, Gun).

% Advanced analysis
?- versatile_weapons(Gun).
```

## Project Statistics

- **121 weapons** from Call of Duty 1 (2003) through Black Ops 6 (2024)
- **49 attachments** across 8 categories with compatibility system
- **26 game mechanics** modeling progression and gameplay systems
- **28 unlock progression chains** for recursive analysis
- **30+ predicates** for comprehensive knowledge querying
- **8 weapon types** including extended categories
- **18 games** spanning 21 years of franchise evolution

## Documentation Standards

This documentation follows these conventions:

- **Predicate notation**: `predicate_name/arity` (e.g., `can_unlock_eventually/2`)
- **Type signatures**: Parameters with their expected types
- **Examples**: Working queries with expected outputs
- **Complexity analysis**: Time and space complexity where relevant
- **Cross-references**: Links between related predicates and concepts

## Advanced Features Covered

### Recursive Algorithms

- Transitive closure computation for weapon progression
- Depth-first search with cycle detection
- Path optimization and longest chain analysis
- Multi-criteria decision making

### Multi-Domain Integration

- Cross-system compatibility analysis
- Era-based trend analysis spanning 21 years
- Complex optimization algorithms
- Statistical pattern recognition

### Prolog Best Practices

- Tail-recursive implementations
- Efficient query optimization
- Proper handling of infinite recursion
- Memory-conscious data structures

This documentation serves as both a reference manual and a guide to understanding the sophisticated logical reasoning, complex data modeling, and comprehensive system integration demonstrated in this knowledgebase.
