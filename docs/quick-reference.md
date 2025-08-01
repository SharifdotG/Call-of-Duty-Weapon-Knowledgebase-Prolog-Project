# Quick Reference Guide

A concise reference for the most commonly used predicates in the Call of Duty Weapon Knowledgebase.

## Basic Weapon Queries

| Predicate | Description | Example |
|-----------|-------------|---------|
| `gun_type/2` | Find weapons by type | `gun_type(assault_rifle, Gun)` |
| `gun_in_game/2` | Find weapons by game | `gun_in_game(modern_warfare, Gun)` |
| `high_damage_guns/1` | High damage weapons | `high_damage_guns(Gun)` |
| `fast_firing_guns/1` | Fast firing weapons | `fast_firing_guns(Gun)` |

## Classification Predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `primary_weapon/1` | Primary weapons | `primary_weapon(Gun)` |
| `close_quarters_weapon/1` | Close quarters weapons | `close_quarters_weapon(Gun)` |
| `modern_era_guns/1` | Modern era weapons | `modern_era_guns(Gun)` |
| `versatile_weapons/1` | Versatile assault rifles | `versatile_weapons(Gun)` |

## Recursive Progression

| Predicate | Description | Example |
|-----------|-------------|---------|
| `can_unlock/2` | Direct unlock | `can_unlock(m4a1, acr)` |
| `can_unlock_eventually/2` | Transitive unlock | `can_unlock_eventually(m4a1, mk14)` |
| `unlock_chain/2` | Complete chain | `unlock_chain(m4a1, Chain)` |
| `progression_depth/2` | Weapon depth | `progression_depth(mk14, Depth)` |

## Attachment System

| Predicate | Description | Example |
|-----------|-------------|---------|
| `compatible_attachments/2` | Compatible attachments | `compatible_attachments(m4a1, Att)` |
| `stealth_build/2` | Stealth attachments | `stealth_build(Gun, Atts)` |
| `optimal_damage_build/2` | Damage build | `optimal_damage_build(Gun, Atts)` |
| `well_supported_weapons/1` | 3+ attachments | `well_supported_weapons(Gun)` |

## Statistical Analysis

| Predicate | Description | Example |
|-----------|-------------|---------|
| `count_weapons_by_type/2` | Count by type | `count_weapons_by_type(pistol, Count)` |
| `most_common_weapon_type/1` | Most common type | `most_common_weapon_type(Type)` |

## Common Query Patterns

### Find weapons matching multiple criteria

```prolog
?- high_damage_guns(Gun), fast_firing_guns(Gun), modern_era_guns(Gun).
```

### Analyze progression chains

```prolog
?- unlock_chain(BaseGun, Chain), length(Chain, Length), Length > 2.
```

### Find optimal builds

```prolog
?- stealth_build(Gun, Atts), length(Atts, Count), Count > 0.
```

### Statistical summaries

```prolog
?- findall(Type-Count, (gun_type(Type, _), count_weapons_by_type(Type, Count)), Stats).
```

## Loading and Testing

```prolog
% Load the knowledgebase
?- consult('src/weapons.pl').

% Run tests
?- consult('tests/weapon_tests.pl'), run_all_tests.

% Run demo
?- consult('demo.pl').

% Quick validation
?- consult('validate.pl').
```
