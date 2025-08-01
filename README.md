# Call of Duty Weapon Knowledgebase - Prolog Project

A comprehensive Prolog knowledgebase modeling iconic Call of Duty weapons from 2003 to 2024, implementing recursive rules for weapon progression and unlocking systems.

## Assignment Context

This project fulfills **Assignment-1: Implement a Basic Knowledgebase of Your Choice Using Prolog** with:

- ✅ **Chosen domain**: Call of Duty weapons (2003-2024)
- ✅ **Relevant facts**: 121 weapon facts, 49 attachment facts, 26 game mechanics
- ✅ **Rules describing key concepts**: Weapon categorization, era classification, attachment systems
- ✅ **Recursive rules**: Weapon unlock progression chains, depth calculation, optimization
- ✅ **Query capabilities**: 30+ predicates for comprehensive knowledge analysis

## Project Structure

```plaintext
├── .github/
│   └── copilot-instructions.md    # Project guidelines and conventions
├── docs/
│   ├── README.md                  # Complete documentation overview
│   ├── predicate-reference.md     # Comprehensive predicate reference
│   ├── recursive-rules.md         # Detailed recursive algorithm analysis
│   ├── technical-implementation.md # System architecture and implementation
│   └── usage-examples.md          # Examples with expected outputs
├── src/
│   └── weapons.pl                 # Main Prolog knowledgebase
├── tests/
│   └── weapon_tests.pl           # Test queries and examples
├── demo.pl                       # Feature demonstration script
├── validate.pl                   # Quick validation script
└── README.md                     # This file
```

## Quick Start

1. **Install SWI-Prolog** (if not already installed)
2. **Load the knowledgebase:**

   ```bash
   swipl src/weapons.pl
   ```

3. **Run sample queries:**

   ```prolog
   ?- gun_type(assault_rifle, Gun).
   ?- can_unlock_eventually(m4a1, Gun).
   ?- unlock_chain(m1_garand, Chain).
   ```

## Key Features

### Weapon Facts

- **121 weapons** from 18 Call of Duty games (2003-2024)
- **Extended weapon types**: Assault rifles, SMGs, sniper rifles, LMGs, pistols, shotguns, marksman rifles, battle rifles
- **Attributes**: Type, ammo, damage tier, fire rate, game
- **Categories**: Primary/secondary, close quarters/long range, automatic weapons

### Attachment System

- **49 attachments** across 8 categories: optics, barrels, stocks, underbarrels, magazines, muzzles, lasers, perks
- **Weapon compatibility**: Rating system (1-5) for attachment-weapon combinations
- **Attachment effects**: Damage boost, recoil reduction, stealth, accuracy improvement
- **Specialized builds**: Stealth, damage, accuracy optimization

### Game Mechanics

- **26 game mechanics** across 5 categories: progression, unlock, modification, gameplay, economy
- **Era classification**: Classic, modern, advanced game mechanics
- **System modeling**: Weapon leveling, prestige, gunsmith, battle pass, etc.

### Recursive Rules

- **Weapon unlock progression chains**: `can_unlock_eventually/2`
- **Attachment progression**: `attachment_unlock_chain/2`
- **Progression depth calculation**: `progression_depth/2`
- **Chain generation**: `unlock_chain/2`
- **Longest chain finding**: `longest_chain_from/2`

### Advanced Analysis

- **Weapon optimization**: Best builds for specific roles (stealth, damage, accuracy)
- **Compatibility analysis**: Well-supported weapons with 3+ attachments
- **Statistical analysis**: Weapon counts by type, most common categories
- **Role-based recommendations**: Close quarters, long range, versatile weapons

## Sample Queries

```prolog
% Find all assault rifles
?- gun_type(assault_rifle, Gun).

% Find high damage, fast firing guns
?- high_damage_guns(Gun), fast_firing_guns(Gun).

% Check if M4A1 can unlock XM4 through progression
?- can_unlock_eventually(m4a1, xm4).

% Get complete unlock chain from M1 Garand
?- unlock_chain(m1_garand, Chain).

% Find progression depth of XM4
?- progression_depth(xm4, Depth).

% Find weapons with stealth builds
?- stealth_build(Gun, Attachments), length(Attachments, Count), Count > 0.

% Find most common weapon type
?- most_common_weapon_type(Type).

% Find well-supported weapons (3+ attachments)
?- well_supported_weapons(Gun).
```

## Games Covered

18 Call of Duty games from 2003-2024:

- Call of Duty series (1, 2)
- Modern Warfare series (2007, 2009, 2011, 2019, 2022, 2023)
- Black Ops series (2010, 2012, 2015, 2018, 2020, 2024)
- Advanced Warfare (2014)
- Infinite Warfare (2016)
- WWII (2017)
- Vanguard (2021)

## Testing

Run the comprehensive test suite:

```prolog
?- consult('tests/weapon_tests.pl').
?- run_all_tests.
```

## Documentation

See `docs/` folder for detailed documentation including:

- Complete predicate reference
- Recursive rule explanations
- Technical implementation details
- Usage examples and expected outputs

## Prolog Conventions

- Snake_case naming for predicates and variables
- Proper fact termination with periods
- Tail-recursive implementations where applicable
- Comprehensive commenting with `%` blocks
- Logical grouping of related predicates

## Assignment Requirements Met

✅ **Domain Choice**: Call of Duty weapons across 21 years of games (2003-2024)
✅ **Relevant Facts**: 121 weapon facts + 49 attachment facts + 26 game mechanics
✅ **Key Concepts**: Weapon types, eras, damage/fire rate classifications, attachment systems, game mechanics
✅ **Recursive Rules**: 8+ recursive predicates for progression analysis, attachment chains, and optimization
✅ **Query Capabilities**: 30+ predicates for comprehensive knowledge manipulation and analysis
✅ **Advanced Features**: Attachment compatibility, weapon optimization, build recommendations, statistical analysis

## License

Educational project for Prolog programming assignment.
