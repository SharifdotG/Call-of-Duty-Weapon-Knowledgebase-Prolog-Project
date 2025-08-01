# Call of Duty Weapon Knowledgebase

This repository is a comprehensive Prolog knowledgebase modeling iconic Call of Duty weapons (2003–2024), their attachments, game mechanics, and complex progression systems.

## Assignment Context

**Assignment-1: Implement a Basic Knowledgebase of Your Choice Using Prolog**

This project fulfills and exceeds the requirements for implementing a Prolog knowledgebase with:

-   A sophisticated domain of knowledge (Call of Duty weapons ecosystem)
-   121 weapon facts, 49 attachment facts, and 26 game mechanic facts
-   Advanced recursive rules for progression analysis and optimization
-   Complex classification and analysis systems
-   Proper use of Prolog syntax and advanced programming techniques

## Project Overview

### Core Systems

**Weapon Database (121 weapons across 18 games)**

-   Model each weapon as `gun(Name, Type, Ammo, DamageTier, FireRate, Game)`
-   Extended weapon types: assault rifles, SMGs, sniper rifles, LMGs, pistols, shotguns, marksman rifles, battle rifles
-   Comprehensive coverage from Call of Duty 1 (2003) through Black Ops 6 (2024)

**Attachment System (49 attachments across 8 categories)**

-   Model as `attachment(Name, Type, Effect, StatModifier, Game)`
-   Categories: optics, barrels, stocks, underbarrels, magazines, muzzles, lasers, perks
-   Compatibility system: `weapon_attachment(WeaponName, AttachmentName, CompatibilityRating)`
-   Attachment effects: damage boost, recoil reduction, stealth, accuracy improvement

**Game Mechanics (26 mechanics across 5 types)**

-   Model as `game_mechanic(MechanicName, Type, Description, ApplicableGames)`
-   Types: progression, unlock, modification, gameplay, economy
-   Era classification: classic, modern, advanced game mechanics

**Progression Systems (28 unlock chains)**

-   `unlock_progression(BaseGun, UpgradedGun)` chains for recursive analysis
-   Attachment progression chains for equipment unlocking
-   Complex depth calculation and optimization algorithms

### Key Features

**Advanced Recursive Rules**

-   `can_unlock_eventually/2`: Transitive closure for weapon progression
-   `unlock_chain/2`: Complete progression chain generation
-   `progression_depth/2`: Recursive depth calculation
-   `longest_chain_from/2`: Optimization and analysis
-   `attachment_unlock_chain/2`: Attachment progression systems

**Classification & Analysis**

-   Weapon categorization (primary/secondary, close quarters/long range)
-   Era-based classification (classic, modern, advanced)
-   Performance analysis (high damage, fast firing, versatile weapons)
-   Statistical analysis and optimization recommendations

**Build Optimization**

-   `optimal_damage_build/2`: Damage-focused attachment combinations
-   `optimal_accuracy_build/2`: Accuracy-enhancing setups
-   `stealth_build/2`: Stealth-optimized configurations
-   Compatibility analysis and rating systems

## File Structure

```
├── .github/
│   └── copilot-instructions.md    # This file - project guidelines
├── src/
│   └── weapons.pl                 # Main Prolog knowledgebase (all systems)
├── tests/
│   └── weapon_tests.pl           # Comprehensive test suite
├── demo.pl                       # Feature demonstration script
├── validate.pl                   # Quick validation script
├── README.md                     # Project documentation
└── .gitignore                    # Version control settings
```

## Implemented Predicates (30+ rules)

### Basic Queries

-   `gun_type/2`, `gun_in_game/2`, `gun_damage/2`, `gun_fire_rate/2`, `gun_ammo_type/2`
-   `high_damage_guns/1`, `fast_firing_guns/1`, `automatic_weapon/1`

### Classification Rules

-   `modern_era_guns/1`, `classic_era_guns/1`, `black_ops_series_guns/1`
-   `primary_weapon/1`, `secondary_weapon/1`, `close_quarters_weapon/1`, `long_range_weapon/1`
-   `pistol_guns/1`, `shotgun_guns/1`, `marksman_rifle_guns/1`, `battle_rifle_guns/1`

### Recursive Progression

-   `can_unlock/2`, `can_unlock_eventually/2`, `unlock_chain/2`, `progression_depth/2`
-   `can_unlock_from/2`, `longest_chain_from/2`, `attachment_unlock_chain/2`

### Attachment System

-   `attachment_type/2`, `attachment_effect/2`, `compatible_attachments/2`
-   `highly_compatible_attachments/2`, `best_attachments/2`, `weapon_optics/2`
-   `damage_boosting_attachments/1`, `accuracy_boosting_attachments/1`, `stealth_attachments/1`

### Game Mechanics

-   `mechanic_type/2`, `progression_mechanics/1`, `unlock_mechanics/1`
-   `modern_mechanics/1`, `classic_mechanics/1`, `gameplay_mechanics/1`

### Advanced Analysis

-   `well_supported_weapons/1`, `versatile_weapons/1`, `best_close_quarters_weapon/1`
-   `optimal_damage_build/2`, `optimal_accuracy_build/2`, `stealth_build/2`
-   `count_weapons_by_type/2`, `most_common_weapon_type/1`, `unique_ammo_weapons/2`

## Prolog Coding Conventions

### Naming & Structure

-   Use snake_case for predicate and variable names (e.g., `can_unlock_eventually`, `WeaponList`)
-   Facts end with periods: `gun(m4a1, assault_rifle, '5.56mm', high, fast, modern_warfare).`
-   Rules grouped by logical sections with blank line separators
-   Complex systems organized into clear sections with headers

### Documentation & Comments

-   Comment blocks before predicate groups using `%`
-   Section headers with `% ==============================`
-   Inline comments for complex logic and recursive base cases
-   Example queries included for demonstration

### Recursive Implementation

-   Use tail-recursive forms where applicable for efficiency
-   Clear base cases for recursive predicates
-   Helper predicates for complex recursive operations
-   Proper handling of infinite recursion prevention

### Data Modeling

-   Consistent fact structure across all domains
-   Meaningful atom names and structured data
-   Compatibility ratings and statistical modifiers
-   Comprehensive coverage with logical organization

## Query Examples & Expected Results

### Basic Weapon Queries

```prolog
% Find all assault rifles
?- gun_type(assault_rifle, Gun).
% Expected: m4a1, ak47, acr, commando, galil, an94, bal27, icr1, nv4, stg44, icr7, m4a1_mw19, ak74, stg44_vg, m4_mw2, svoa_545, xm4

% Find high damage, fast firing guns
?- high_damage_guns(Gun), fast_firing_guns(Gun).
% Expected: m4a1, ppsh_41, mg42, m249_saw, acr, commando, etc.
```

### Recursive Progression

```prolog
% Check if M4A1 can unlock XM4 through progression
?- can_unlock_eventually(m4a1, xm4).
% Expected: true

% Get complete unlock chain from M4A1
?- unlock_chain(m4a1, Chain).
% Expected: [m4a1, acr, mk14]

% Find progression depth of XM4
?- progression_depth(xm4, Depth).
% Expected: Depth = 6
```

### Advanced Analysis

```prolog
% Find weapons with optimal stealth builds
?- stealth_build(Gun, Attachments), length(Attachments, Count), Count > 0.
% Expected: Multiple weapon-attachment combinations

% Find most versatile assault rifles
?- versatile_weapons(Gun).
% Expected: Weapons that are high damage and well-supported

% Analyze weapon statistics
?- most_common_weapon_type(Type).
% Expected: Type = assault_rifle (or current most common)
```

## Testing & Validation

### Test Suite Execution

```bash
# Load and run comprehensive tests
swipl tests/weapon_tests.pl
?- run_all_tests.

# Quick validation
swipl validate.pl

# Feature demonstration
swipl demo.pl
```

### Expected Test Coverage

-   Basic fact verification (121 weapons, 49 attachments, 26 mechanics)
-   Rule functionality across all 30+ predicates
-   Recursive progression correctness and termination
-   Classification accuracy and completeness
-   Attachment compatibility and optimization
-   Statistical analysis and edge cases

## Advanced Features Demonstrated

### Multi-Domain Knowledge Integration

-   Cross-referencing weapons, attachments, and game mechanics
-   Era-based analysis spanning 21 years of game development
-   Complex compatibility and optimization algorithms

### Sophisticated Recursive Algorithms

-   Transitive closure computation for progression chains
-   Depth-first search with cycle detection
-   Optimization through longest path analysis
-   Multi-criteria decision making for build recommendations

### Statistical & Analytical Capabilities

-   Comprehensive counting and classification
-   Pattern recognition across weapon categories
-   Optimization recommendations based on multiple criteria
-   Trend analysis across game eras and mechanical evolution

This knowledgebase represents a sophisticated implementation that goes well beyond basic Prolog programming, demonstrating advanced logical reasoning, complex data modeling, and comprehensive system integration.
