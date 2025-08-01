# Usage Examples and Expected Outputs

This document provides comprehensive examples of using the Call of Duty Weapon Knowledgebase, with detailed expected outputs and practical use cases.

## Getting Started

### Loading the Knowledgebase

```prolog
% Start SWI-Prolog and load the knowledgebase
?- consult('src/weapons.pl').
% Loading /path/to/src/weapons.pl
% true.
```

### Basic Validation

```prolog
% Quick validation that the system is working
?- gun(m4a1, assault_rifle, '5.56mm', high, fast, modern_warfare).
true.

% Check total number of weapons
?- findall(Gun, gun(Gun, _, _, _, _, _), Guns), length(Guns, Count).
Guns = [m1_garand, kar98k, thompson, sten, bar, m1911, trench_gun, ...],
Count = 121.
```

## Basic Weapon Queries

### Finding Weapons by Type

```prolog
% Find all assault rifles
?- gun_type(assault_rifle, Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = acr ;
Gun = commando ;
Gun = galil ;
Gun = an94 ;
Gun = bal27 ;
Gun = icr1 ;
Gun = nv4 ;
Gun = stg44 ;
Gun = icr7 ;
Gun = m4a1_mw19 ;
Gun = ak74 ;
Gun = stg44_vg ;
Gun = m4_mw2 ;
Gun = svoa_545 ;
Gun = xm4.

% Count assault rifles
?- findall(Gun, gun_type(assault_rifle, Gun), ARs), length(ARs, Count).
ARs = [m4a1, ak47, acr, commando, galil, an94, bal27, icr1, nv4, stg44, icr7, m4a1_mw19, ak74, stg44_vg, m4_mw2, svoa_545, xm4],
Count = 17.
```

### Finding Weapons by Game

```prolog
% Find all weapons from Modern Warfare (2007)
?- gun_in_game(modern_warfare, Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = mp5 ;
Gun = m249_saw ;
Gun = m40a3 ;
Gun = desert_eagle ;
Gun = w1200.

% Find which game contains a specific weapon
?- gun_in_game(Game, intervention).
Game = modern_warfare_2.
```

### Performance-Based Queries

```prolog
% Find high damage weapons
?- high_damage_guns(Gun).
Gun = m1_garand ;
Gun = kar98k ;
Gun = m4a1 ;
Gun = ak47 ;
Gun = m40a3 ;
Gun = desert_eagle ;
... (continues for all high/very_high damage weapons)

% Find fast firing weapons
?- fast_firing_guns(Gun).
Gun = thompson ;
Gun = sten ;
Gun = ppsh_41 ;
Gun = mg42 ;
Gun = m4a1 ;
Gun = mp5 ;
... (continues for all fast/very_fast weapons)

% Find weapons that are both high damage AND fast firing
?- high_damage_guns(Gun), fast_firing_guns(Gun).
Gun = m4a1 ;
Gun = ppsh_41 ;
Gun = mg42 ;
Gun = m249_saw ;
Gun = acr ;
Gun = commando ;
Gun = mp7 ;
Gun = m4a1_mw19 ;
Gun = mp5_mw19 ;
Gun = ak74 ;
Gun = mp5_cw ;
Gun = stg44_vg ;
Gun = m4_mw2 ;
Gun = svoa_545.
```

## Era and Classification Queries

### Era-Based Analysis

```prolog
% Find modern era weapons
?- modern_era_guns(Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = mp5 ;
... (all Modern Warfare series weapons)

% Find classic era weapons
?- classic_era_guns(Gun).
Gun = m1_garand ;
Gun = kar98k ;
Gun = thompson ;
... (Call of Duty 1, 2, WWII, Vanguard weapons)

% Count weapons by era
?- findall(Gun, modern_era_guns(Gun), ModernGuns), length(ModernGuns, ModernCount).
ModernGuns = [m4a1, ak47, mp5, m249_saw, m40a3, desert_eagle, w1200, acr, intervention, ump45, rpd, usp_45, aa12, mk14, mp7, msr, five_seven, model1887, m4a1_mw19, mp5_mw19, ax50, mk2_carbine, kar98k_mw, spr208, fal, oden, fr556, x16, model680, m4_mw2, lachmann_sub, la_b_330, sa_b50, lockwood_mk2, ftac_recon, lachmann762, p890, expedite12, svoa_545, wsm9, longbow_sniper, tempus_torrent, mcw_68, cronen_squall, bas_b, renetti, lockwood300],
ModernCount = 45.
```

### Weapon Classification

```prolog
% Find primary weapons
?- findall(Gun, primary_weapon(Gun), PrimaryWeapons), length(PrimaryWeapons, Count).
PrimaryWeapons = [m1_garand, kar98k, thompson, sten, bar, trench_gun, mosin_nagant, ppsh_41, mg42, panzerfaust, double_barrel, m4a1, ak47, mp5, m249_saw, m40a3, w1200, ...],
Count = 110.

% Find close quarters weapons
?- close_quarters_weapon(Gun).
Gun = thompson ;
Gun = sten ;
Gun = trench_gun ;
Gun = ppsh_41 ;
Gun = double_barrel ;
Gun = mp5 ;
Gun = w1200 ;
... (all SMGs, shotguns, pistols)

% Find long range weapons
?- long_range_weapon(Gun).
Gun = m40a3 ;
Gun = intervention ;
Gun = l96a1 ;
Gun = msr ;
Gun = dsr50 ;
Gun = mors ;
... (all sniper rifles, marksman rifles, battle rifles)
```

## Recursive Progression Queries

### Direct Unlock Relationships

```prolog
% Check direct unlock relationship
?- can_unlock(m4a1, acr).
true.

?- can_unlock(m4a1, mk14).
false.

% Find what M4A1 can directly unlock
?- can_unlock(m4a1, Gun).
Gun = acr.

% Find what can directly unlock ACR
?- can_unlock(Gun, acr).
Gun = m4a1.
```

### Transitive Progression Analysis

```prolog
% Check if M4A1 can eventually unlock MK14
?- can_unlock_eventually(m4a1, mk14).
true.

% Find all weapons M4A1 can eventually unlock
?- can_unlock_eventually(m4a1, Gun).
Gun = acr ;
Gun = mk14.

% Find all weapons that can eventually unlock XM4
?- can_unlock_eventually(Gun, xm4).
Gun = svoa_545 ;
Gun = m4_mw2 ;
Gun = stg44_vg ;
Gun = ak74 ;
Gun = m4a1_mw19 ;
Gun = icr7 ;
Gun = stg44 ;
Gun = nv4 ;
Gun = icr1 ;
Gun = bal27 ;
Gun = an94 ;
Gun = mk14 ;
Gun = acr ;
Gun = m4a1.
```

### Unlock Chain Generation

```prolog
% Get complete unlock chain from M4A1
?- unlock_chain(m4a1, Chain).
Chain = [m4a1, acr, mk14].

% Get complete unlock chain from M1 Garand
?- unlock_chain(m1_garand, Chain).
Chain = [m1_garand, thompson, bar].

% Find longest chain in the system
?- unlock_chain(m4a1, Chain1), unlock_chain(m1_garand, Chain2),
   length(Chain1, Len1), length(Chain2, Len2),
   (Len1 >= Len2 -> Longest = Chain1 ; Longest = Chain2).
Chain1 = [m4a1, acr, mk14],
Chain2 = [m1_garand, thompson, bar],
Len1 = 3,
Len2 = 3,
Longest = [m4a1, acr, mk14].
```

### Progression Depth Analysis

```prolog
% Find progression depth of various weapons
?- progression_depth(m4a1, Depth).
Depth = 0.

?- progression_depth(acr, Depth).
Depth = 1.

?- progression_depth(mk14, Depth).
Depth = 2.

% Find the deepest weapon in progression
?- progression_depth(xm4, Depth).
Depth = 13.

% Find all weapons at each depth level
?- progression_depth(Gun, 0).
Gun = m1_garand ;
Gun = kar98k ;
Gun = m4a1 ;
Gun = ak47 ;
Gun = mp5 ;
Gun = m40a3 ;
Gun = an94 ;
Gun = red_dot_sight ;
Gun = foregrip ;
Gun = suppressor.

?- findall(Gun, progression_depth(Gun, 1), Depth1Weapons).
Depth1Weapons = [thompson, mosin_nagant, acr, commando, ump45, intervention, bal27, acog_scope, bipod, compensator].
```

## Attachment System Queries

### Basic Attachment Queries

```prolog
% Find all optic attachments
?- attachment_type(optic, Attachment).
Attachment = red_dot_sight ;
Attachment = acog_scope ;
Attachment = thermal_scope ;
Attachment = holographic_sight ;
Attachment = reflex_sight ;
Attachment = variable_zoom ;
Attachment = scout_scope ;
Attachment = iron_sights.

% Find attachments with damage boost effects
?- damage_boosting_attachments(Attachment).
Attachment = heavy_barrel ;
Attachment = stopping_power ;
Attachment = fmj ;
Attachment = deep_impact ;
Attachment = hardened.
```

### Weapon-Attachment Compatibility

```prolog
% Find all attachments compatible with M4A1
?- compatible_attachments(m4a1, Attachment).
Attachment = acog_scope ;
Attachment = foregrip ;
Attachment = suppressor ;
Attachment = extended_mag ;
Attachment = red_dot_sight ;
Attachment = tactical_stock.

% Find highly compatible attachments for M4A1 (rating >= 4)
?- highly_compatible_attachments(m4a1, Attachment).
Attachment = acog_scope ;
Attachment = foregrip ;
Attachment = extended_mag ;
Attachment = red_dot_sight ;
Attachment = tactical_stock.

% Find perfect attachments for M4A1 (rating = 5)
?- best_attachments(m4a1, Attachment).
Attachment = acog_scope ;
Attachment = suppressor ;
Attachment = red_dot_sight.
```

### Build Optimization

```prolog
% Find optimal damage build for AK-47
?- optimal_damage_build(ak47, Attachments).
Attachments = [heavy_barrel, compensator].

% Find stealth build for M4A1
?- stealth_build(m4a1, Attachments).
Attachments = [suppressor].

% Find optimal accuracy build for Intervention
?- optimal_accuracy_build(intervention, Attachments).
Attachments = [].

% Check if weapon has any stealth attachments
?- stealth_build(mp5, Attachments), length(Attachments, Count), Count > 0.
Attachments = [suppressor],
Count = 1.
```

## Statistical Analysis

### Weapon Statistics

```prolog
% Count weapons by type
?- count_weapons_by_type(assault_rifle, Count).
Count = 17.

?- count_weapons_by_type(pistol, Count).
Count = 20.

?- count_weapons_by_type(shotgun, Count).
Count = 19.

% Find most common weapon type
?- most_common_weapon_type(Type).
Type = pistol.

% Get statistics for all weapon types
?- findall(Type-Count, (
    gun_type(Type, _),
    count_weapons_by_type(Type, Count)
   ), TypeStats).
TypeStats = [rifle-7, submachine_gun-13, light_machine_gun-3, pistol-20, shotgun-19, machine_gun-1, rocket_launcher-1, assault_rifle-17, sniper_rifle-16, marksman_rifle-12, battle_rifle-12].
```

### Advanced Analysis

```prolog
% Find well-supported weapons (3+ attachments)
?- well_supported_weapons(Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = intervention ;
Gun = m40a3 ;
Gun = mp5 ;
Gun = ump45 ;
Gun = m249_saw ;
Gun = w1200 ;
Gun = desert_eagle.

% Count well-supported weapons
?- findall(Gun, well_supported_weapons(Gun), WSWeapons), length(WSWeapons, Count).
WSWeapons = [m4a1, ak47, intervention, m40a3, mp5, ump45, m249_saw, w1200, desert_eagle],
Count = 9.

% Find versatile weapons
?- versatile_weapons(Gun).
Gun = m4a1 ;
Gun = ak74.

% Find best close quarters weapons
?- best_close_quarters_weapon(Gun).
Gun = ppsh_41 ;
Gun = mg42 ;
Gun = mp7.
```

## Game Mechanics Analysis

### Mechanic Classification

```prolog
% Find all progression mechanics
?- progression_mechanics(Mechanic).
Mechanic = weapon_leveling ;
Mechanic = prestige_system ;
Mechanic = weapon_mastery ;
Mechanic = battle_pass ;
Mechanic = weapon_blueprints ;
Mechanic = camo_challenges.

% Find modern mechanics
?- modern_mechanics(Mechanic).
Mechanic = weapon_leveling ;
Mechanic = weapon_mastery ;
Mechanic = weapon_blueprints ;
Mechanic = challenge_unlock ;
Mechanic = currency_unlock ;
Mechanic = attachment_progression ;
Mechanic = gunsmith ;
Mechanic = weapon_variants ;
Mechanic = attachment_tuning ;
Mechanic = weapon_inspection ;
Mechanic = reload_cancel ;
Mechanic = tactical_reload ;
Mechanic = weapon_mounting ;
Mechanic = slide_cancel ;
Mechanic = direct_purchase.

% Count mechanics by type
?- findall(Type-Count, (
    mechanic_type(Type, _),
    findall(M, mechanic_type(Type, M), Mechs),
    length(Mechs, Count)
   ), MechStats).
MechStats = [progression-6, unlock-5, modification-5, gameplay-6, economy-4].
```

## Complex Combined Queries

### Multi-Criteria Weapon Selection

```prolog
% Find assault rifles from modern games with high damage
?- gun_type(assault_rifle, Gun),
   modern_era_guns(Gun),
   high_damage_guns(Gun).
Gun = m4a1 ;
Gun = ak47 ;
Gun = acr ;
Gun = mk14 ;
Gun = m4a1_mw19 ;
Gun = fal ;
Gun = oden ;
Gun = fr556 ;
Gun = ftac_recon ;
Gun = lachmann762 ;
Gun = svoa_545 ;
Gun = cronen_squall ;
Gun = bas_b.

% Find weapons that are automatic, high damage, and well-supported
?- automatic_weapon(Gun),
   high_damage_guns(Gun),
   well_supported_weapons(Gun).
Gun = m4a1 ;
Gun = mg42 ;
Gun = m249_saw.
```

### Progression Analysis

```prolog
% Find all possible end-chain weapons (maximum depth)
?- progression_depth(Gun, Depth),
   \+ unlock_progression(Gun, _),
   Depth > 0.
Gun = bar, Depth = 2 ;
Gun = mg42, Depth = 4 ;
Gun = mk14, Depth = 2 ;
Gun = galil, Depth = 2 ;
Gun = mp7, Depth = 2 ;
Gun = msr, Depth = 2 ;
Gun = xm4, Depth = 13 ;
Gun = thermal_scope, Depth = 2 ;
Gun = compensator, Depth = 1.

% Analyze progression tree statistics
?- findall(Depth, progression_depth(_, Depth), Depths),
   max_list(Depths, MaxDepth),
   min_list(Depths, MinDepth).
Depths = [0, 0, 1, 2, 0, 0, 0, 1, 4, 0, 0, 0, 3, 2, 0, 0, 0, 1, 2, 0, 0, 0, 1, 2, 0, 13, 0, 1, 2],
MaxDepth = 13,
MinDepth = 0.
```

### Attachment Analysis

```prolog
% Find weapons with the most attachment options
?- findall(Gun-Count, (
    gun(Gun, _, _, _, _, _),
    findall(A, compatible_attachments(Gun, A), Attachments),
    length(Attachments, Count),
    Count > 0
   ), WeaponAttachmentCounts),
   sort(WeaponAttachmentCounts, Sorted),
   reverse(Sorted, BestFirst).
WeaponAttachmentCounts = [m4a1-6, ak47-5, intervention-4, m40a3-3, mp5-5, ump45-2, m249_saw-4, w1200-3, desert_eagle-3],
BestFirst = [m4a1-6, ak47-5, mp5-5, intervention-4, m249_saw-4, m40a3-3, w1200-3, desert_eagle-3, ump45-2].

% Find attachment types with most options
?- findall(Type-Count, (
    attachment_type(Type, _),
    findall(A, attachment_type(Type, A), Attachments),
    length(Attachments, Count)
   ), AttachmentTypeStats).
AttachmentTypeStats = [optic-8, barrel-7, stock-6, underbarrel-6, magazine-6, muzzle-6, laser-4, perk-6].
```

These examples demonstrate the comprehensive querying capabilities of the knowledgebase, from simple fact retrieval to complex multi-criteria analysis and statistical computations.
