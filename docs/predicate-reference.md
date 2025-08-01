# Complete Predicate Reference

This document provides a comprehensive reference for all predicates implemented in the Call of Duty Weapon Knowledgebase.

## Basic Query Predicates

### Weapon Information Queries

#### `gun_type/2`

**Syntax**: `gun_type(?Type, ?Gun)`
**Description**: Relates weapons to their types
**Parameters**:

- `Type`: Weapon type (assault_rifle, submachine_gun, sniper_rifle, etc.)
- `Gun`: Weapon name

**Examples**:

```prolog
% Find all assault rifles
?- gun_type(assault_rifle, Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = acr ;
...

% Check if M4A1 is an assault rifle
?- gun_type(assault_rifle, m4a1).
true.

% Find the type of M4A1
?- gun_type(Type, m4a1).
Type = assault_rifle.
```

#### `gun_in_game/2`

**Syntax**: `gun_in_game(?Game, ?Gun)`
**Description**: Relates weapons to the games they appear in
**Parameters**:

- `Game`: Call of Duty game name
- `Gun`: Weapon name

**Examples**:

```prolog
% Find all weapons in Modern Warfare
?- gun_in_game(modern_warfare, Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = mp5 ;
...

% Check which game contains M4A1
?- gun_in_game(Game, m4a1).
Game = modern_warfare.
```

#### `gun_damage/2`

**Syntax**: `gun_damage(?Damage, ?Gun)`
**Description**: Relates weapons to their damage tiers
**Parameters**:

- `Damage`: Damage tier (low, medium, high, very_high)
- `Gun`: Weapon name

#### `gun_fire_rate/2`

**Syntax**: `gun_fire_rate(?FireRate, ?Gun)`
**Description**: Relates weapons to their fire rates
**Parameters**:

- `FireRate`: Fire rate (slow, medium, fast, very_fast)
- `Gun`: Weapon name

#### `gun_ammo_type/2`

**Syntax**: `gun_ammo_type(?Ammo, ?Gun)`
**Description**: Relates weapons to their ammunition types
**Parameters**:

- `Ammo`: Ammunition type ('5.56mm', '7.62mm', etc.)
- `Gun`: Weapon name

## Classification Predicates

### Performance-Based Classification

#### `high_damage_guns/1`

**Syntax**: `high_damage_guns(?Gun)`
**Description**: Identifies weapons with high or very high damage
**Parameters**:

- `Gun`: Weapon name

**Examples**:

```prolog
% Find all high damage weapons
?- high_damage_guns(Gun).
Gun = m1_garand ;
Gun = kar98k ;
Gun = m4a1 ;
...
```

#### `fast_firing_guns/1`

**Syntax**: `fast_firing_guns(?Gun)`
**Description**: Identifies weapons with fast or very fast fire rates
**Parameters**:

- `Gun`: Weapon name

#### `automatic_weapon/1`

**Syntax**: `automatic_weapon(?Gun)`
**Description**: Identifies automatic weapons (assault rifles, SMGs, LMGs with fast fire rates)
**Parameters**:

- `Gun`: Weapon name

### Era-Based Classification

#### `modern_era_guns/1`

**Syntax**: `modern_era_guns(?Gun)`
**Description**: Identifies weapons from Modern Warfare series games
**Parameters**:

- `Gun`: Weapon name

#### `classic_era_guns/1`

**Syntax**: `classic_era_guns(?Gun)`
**Description**: Identifies weapons from classic Call of Duty games (1, 2, WWII, Vanguard)
**Parameters**:

- `Gun`: Weapon name

#### `black_ops_series_guns/1`

**Syntax**: `black_ops_series_guns(?Gun)`
**Description**: Identifies weapons from Black Ops series games
**Parameters**:

- `Gun`: Weapon name

### Type-Based Classification

#### `primary_weapon/1`

**Syntax**: `primary_weapon(?Gun)`
**Description**: Identifies primary weapons (assault rifles, sniper rifles, LMGs, etc.)
**Parameters**:

- `Gun`: Weapon name

#### `secondary_weapon/1`

**Syntax**: `secondary_weapon(?Gun)`
**Description**: Identifies secondary weapons (pistols, some SMGs)
**Parameters**:

- `Gun`: Weapon name

#### `close_quarters_weapon/1`

**Syntax**: `close_quarters_weapon(?Gun)`
**Description**: Identifies weapons suitable for close-quarters combat
**Parameters**:

- `Gun`: Weapon name

#### `long_range_weapon/1`

**Syntax**: `long_range_weapon(?Gun)`
**Description**: Identifies weapons suitable for long-range combat
**Parameters**:

- `Gun`: Weapon name

### Extended Type Queries

#### `pistol_guns/1`

**Syntax**: `pistol_guns(?Gun)`
**Description**: Identifies all pistol weapons
**Parameters**:

- `Gun`: Weapon name

#### `shotgun_guns/1`

**Syntax**: `shotgun_guns(?Gun)`
**Description**: Identifies all shotgun weapons
**Parameters**:

- `Gun`: Weapon name

#### `marksman_rifle_guns/1`

**Syntax**: `marksman_rifle_guns(?Gun)`
**Description**: Identifies all marksman rifle weapons
**Parameters**:

- `Gun`: Weapon name

#### `battle_rifle_guns/1`

**Syntax**: `battle_rifle_guns(?Gun)`
**Description**: Identifies all battle rifle weapons
**Parameters**:

- `Gun`: Weapon name

## Recursive Progression Predicates

### Core Progression Rules

#### `can_unlock/2`

**Syntax**: `can_unlock(?BaseGun, ?NextGun)`
**Description**: Direct unlock relationship between weapons
**Parameters**:

- `BaseGun`: Base weapon that can unlock another
- `NextGun`: Weapon that can be unlocked

**Examples**:

```prolog
% Check if M4A1 can directly unlock ACR
?- can_unlock(m4a1, acr).
true.

% Find what M4A1 can directly unlock
?- can_unlock(m4a1, Gun).
Gun = acr.
```

#### `can_unlock_eventually/2`

**Syntax**: `can_unlock_eventually(?BaseGun, ?TargetGun)`
**Description**: Transitive closure of unlock relationship - can unlock through progression chain
**Complexity**: O(n) where n is chain length
**Parameters**:

- `BaseGun`: Starting weapon
- `TargetGun`: Target weapon to unlock

**Examples**:

```prolog
% Check if M4A1 can eventually unlock XM4
?- can_unlock_eventually(m4a1, xm4).
true.

% Find all weapons M4A1 can eventually unlock
?- can_unlock_eventually(m4a1, Gun).
Gun = acr ;
Gun = mk14 ;
false.
```

#### `unlock_chain/2`

**Syntax**: `unlock_chain(?BaseGun, ?Chain)`
**Description**: Generates complete unlock chain starting from a weapon
**Parameters**:

- `BaseGun`: Starting weapon
- `Chain`: List of weapons in unlock order

**Examples**:

```prolog
% Get unlock chain from M4A1
?- unlock_chain(m4a1, Chain).
Chain = [m4a1, acr, mk14].

% Get unlock chain from M1 Garand
?- unlock_chain(m1_garand, Chain).
Chain = [m1_garand, thompson, bar].
```

#### `progression_depth/2`

**Syntax**: `progression_depth(?Gun, ?Depth)`
**Description**: Calculates progression depth (distance from base weapons)
**Complexity**: O(d) where d is depth
**Parameters**:

- `Gun`: Weapon name
- `Depth`: Numerical depth (0 for base weapons)

**Examples**:

```prolog
% Find depth of M4A1 (base weapon)
?- progression_depth(m4a1, Depth).
Depth = 0.

% Find depth of MK14
?- progression_depth(mk14, Depth).
Depth = 2.

% Find all weapons at depth 0
?- progression_depth(Gun, 0).
Gun = m1_garand ;
Gun = kar98k ;
Gun = m4a1 ;
...
```

### Advanced Progression Analysis

#### `can_unlock_from/2`

**Syntax**: `can_unlock_from(?TargetGun, ?BaseGun)`
**Description**: Finds all weapons that can unlock a target weapon
**Parameters**:

- `TargetGun`: Target weapon
- `BaseGun`: Base weapon that can unlock target

#### `longest_chain_from/2`

**Syntax**: `longest_chain_from(?Gun, ?Chain)`
**Description**: Finds the longest unlock chain starting from a weapon
**Complexity**: O(n*m) where n is number of chains, m is average chain length
**Parameters**:

- `Gun`: Starting weapon
- `Chain`: Longest chain as list

## Attachment System Predicates

### Basic Attachment Queries

#### `attachment_type/2`

**Syntax**: `attachment_type(?Type, ?Attachment)`
**Description**: Relates attachments to their types
**Parameters**:

- `Type`: Attachment type (optic, barrel, stock, etc.)
- `Attachment`: Attachment name

#### `attachment_effect/2`

**Syntax**: `attachment_effect(?Effect, ?Attachment)`
**Description**: Relates attachments to their effects
**Parameters**:

- `Effect`: Effect type (damage_boost, stealth, etc.)
- `Attachment`: Attachment name

#### `compatible_attachments/2`

**Syntax**: `compatible_attachments(?Weapon, ?Attachment)`
**Description**: Finds attachments compatible with a weapon
**Parameters**:

- `Weapon`: Weapon name
- `Attachment`: Compatible attachment

**Examples**:

```prolog
% Find all attachments compatible with M4A1
?- compatible_attachments(m4a1, Attachment).
Attachment = acog_scope ;
Attachment = foregrip ;
Attachment = suppressor ;
...
```

#### `highly_compatible_attachments/2`

**Syntax**: `highly_compatible_attachments(?Weapon, ?Attachment)`
**Description**: Finds highly compatible attachments (rating >= 4)
**Parameters**:

- `Weapon`: Weapon name
- `Attachment`: Highly compatible attachment

#### `best_attachments/2`

**Syntax**: `best_attachments(?Weapon, ?Attachment)`
**Description**: Finds perfectly compatible attachments (rating = 5)
**Parameters**:

- `Weapon`: Weapon name
- `Attachment`: Best-rated attachment

### Specialized Attachment Queries

#### `weapon_optics/2`

**Syntax**: `weapon_optics(?Weapon, ?Optic)`
**Description**: Finds optic attachments for a weapon
**Parameters**:

- `Weapon`: Weapon name
- `Optic`: Compatible optic attachment

#### `damage_boosting_attachments/1`

**Syntax**: `damage_boosting_attachments(?Attachment)`
**Description**: Identifies attachments that boost damage
**Parameters**:

- `Attachment`: Damage-boosting attachment

#### `accuracy_boosting_attachments/1`

**Syntax**: `accuracy_boosting_attachments(?Attachment)`
**Description**: Identifies attachments that boost accuracy
**Parameters**:

- `Attachment`: Accuracy-boosting attachment

#### `stealth_attachments/1`

**Syntax**: `stealth_attachments(?Attachment)`
**Description**: Identifies attachments that provide stealth benefits
**Parameters**:

- `Attachment`: Stealth attachment

### Attachment Progression

#### `attachment_unlock_chain/2`

**Syntax**: `attachment_unlock_chain(?BaseAttachment, ?Chain)`
**Description**: Generates attachment unlock progression chains
**Parameters**:

- `BaseAttachment`: Starting attachment
- `Chain`: List of attachments in unlock order

#### `can_unlock_attachment_eventually/2`

**Syntax**: `can_unlock_attachment_eventually(?BaseAttachment, ?TargetAttachment)`
**Description**: Transitive closure for attachment progression
**Parameters**:

- `BaseAttachment`: Starting attachment
- `TargetAttachment`: Target attachment to unlock

## Game Mechanics Predicates

### Basic Mechanics Queries

#### `mechanic_type/2`

**Syntax**: `mechanic_type(?Type, ?Mechanic)`
**Description**: Relates game mechanics to their types
**Parameters**:

- `Type`: Mechanic type (progression, unlock, modification, etc.)
- `Mechanic`: Game mechanic name

#### `progression_mechanics/1`

**Syntax**: `progression_mechanics(?Mechanic)`
**Description**: Identifies progression-related mechanics
**Parameters**:

- `Mechanic`: Progression mechanic

#### `unlock_mechanics/1`

**Syntax**: `unlock_mechanics(?Mechanic)`
**Description**: Identifies unlock-related mechanics
**Parameters**:

- `Mechanic`: Unlock mechanic

#### `modern_mechanics/1`

**Syntax**: `modern_mechanics(?Mechanic)`
**Description**: Identifies mechanics from modern games
**Parameters**:

- `Mechanic`: Modern game mechanic

#### `classic_mechanics/1`

**Syntax**: `classic_mechanics(?Mechanic)`
**Description**: Identifies mechanics from classic games
**Parameters**:

- `Mechanic`: Classic game mechanic

## Advanced Analysis Predicates

### Weapon Analysis

#### `well_supported_weapons/1`

**Syntax**: `well_supported_weapons(?Weapon)`
**Description**: Finds weapons with 3+ compatible attachments
**Parameters**:

- `Weapon`: Well-supported weapon

#### `versatile_weapons/1`

**Syntax**: `versatile_weapons(?Weapon)`
**Description**: Finds versatile assault rifles (high damage + well-supported)
**Parameters**:

- `Weapon`: Versatile weapon

#### `best_close_quarters_weapon/1`

**Syntax**: `best_close_quarters_weapon(?Weapon)`
**Description**: Finds optimal close quarters weapons
**Parameters**:

- `Weapon`: Best CQ weapon

### Build Optimization

#### `optimal_damage_build/2`

**Syntax**: `optimal_damage_build(?Weapon, ?Attachments)`
**Description**: Finds optimal damage-focused attachment combination
**Parameters**:

- `Weapon`: Weapon name
- `Attachments`: List of damage-optimized attachments

**Examples**:

```prolog
% Find optimal damage build for AK-47
?- optimal_damage_build(ak47, Attachments).
Attachments = [heavy_barrel, compensator, drum_mag].
```

#### `optimal_accuracy_build/2`

**Syntax**: `optimal_accuracy_build(?Weapon, ?Attachments)`
**Description**: Finds optimal accuracy-focused attachment combination
**Parameters**:

- `Weapon`: Weapon name
- `Attachments`: List of accuracy-optimized attachments

#### `stealth_build/2`

**Syntax**: `stealth_build(?Weapon, ?Attachments)`
**Description**: Finds stealth-optimized attachment combination
**Parameters**:

- `Weapon`: Weapon name
- `Attachments`: List of stealth attachments

**Examples**:

```prolog
% Find stealth build for M4A1
?- stealth_build(m4a1, Attachments).
Attachments = [suppressor].
```

### Statistical Analysis

#### `count_weapons_by_type/2`

**Syntax**: `count_weapons_by_type(?Type, ?Count)`
**Description**: Counts weapons by type
**Parameters**:

- `Type`: Weapon type
- `Count`: Number of weapons of that type

**Examples**:

```prolog
% Count assault rifles
?- count_weapons_by_type(assault_rifle, Count).
Count = 17.
```

#### `most_common_weapon_type/1`

**Syntax**: `most_common_weapon_type(?Type)`
**Description**: Finds the most common weapon type
**Parameters**:

- `Type`: Most common weapon type

#### `unique_ammo_weapons/2`

**Syntax**: `unique_ammo_weapons(?Gun, ?Ammo)`
**Description**: Finds weapons with unique ammunition types
**Parameters**:

- `Gun`: Weapon with unique ammo
- `Ammo`: Unique ammunition type

## Utility Predicates

### Data Manipulation

#### `guns_by_era/2`

**Syntax**: `guns_by_era(?Era, ?Guns)`
**Description**: Groups weapons by era
**Parameters**:

- `Era`: Era/game name
- `Guns`: List of weapons from that era

#### `guns_by_type/2`

**Syntax**: `guns_by_type(?Type, ?Guns)`
**Description**: Groups weapons by type
**Parameters**:

- `Type`: Weapon type
- `Guns`: List of weapons of that type

#### `weapon_stats/6`

**Syntax**: `weapon_stats(?Gun, ?Type, ?Ammo, ?Damage, ?FireRate, ?Game)`
**Description**: Retrieves complete weapon statistics
**Parameters**:

- `Gun`: Weapon name
- `Type`: Weapon type
- `Ammo`: Ammunition type
- `Damage`: Damage tier
- `FireRate`: Fire rate
- `Game`: Game of origin

## Performance Notes

### Time Complexity

- **Basic queries**: O(1) for fact lookups
- **Classification predicates**: O(n) where n is number of weapons
- **Recursive progression**: O(d) where d is chain depth
- **Chain generation**: O(n) where n is chain length
- **Statistical analysis**: O(n) for counting operations

### Space Complexity

- **Fact storage**: O(1) per fact
- **Chain storage**: O(n) where n is chain length
- **Attachment compatibility**: O(w*a) where w is weapons, a is attachments

### Optimization Tips

- Use specific bindings when possible to reduce search space
- For statistical queries, consider caching results
- Recursive queries benefit from tail-call optimization
- Large result sets may benefit from findall/3 for collection
