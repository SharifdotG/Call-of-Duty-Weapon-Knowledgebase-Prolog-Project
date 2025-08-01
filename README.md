# Call of Duty Weapon Knowledgebase - Prolog Project

A comprehensive Prolog knowledgebase modeling iconic Call of Duty weapons from 2003 to 2024, implementing recursive rules for weapon progression and unlocking systems.

## Project Overview

This project demonstrates advanced Prolog programming techniques through a sophisticated knowledgebase featuring:

- 🎯 **Comprehensive domain**: Call of Duty weapons ecosystem (2003-2024)
- 📊 **Rich data model**: 121 weapon facts, 49 attachment facts, 26 game mechanics
- 🔧 **Advanced rules**: Weapon categorization, era classification, attachment systems
- 🔄 **Recursive algorithms**: Weapon unlock progression chains, depth calculation, optimization
- 🔍 **Query capabilities**: 30+ predicates for comprehensive knowledge analysis

## Project Structure

```plaintext
├── .github/
│   └── copilot-instructions.md    # Project guidelines and conventions
├── diagrams/
│   ├── knowledgebase-diagram-horizontal.png      # System architecture (horizontal)
│   ├── knowledgebase-diagram-vertical.png        # System architecture (vertical)
│   ├── weapon-type-family-classification-*.png   # Weapon classification diagrams
│   ├── weapon-progression-family-tree-*.png      # Weapon progression trees
│   ├── attachment-progression-family-tree-*.png  # Attachment progression trees
│   ├── game-mechanics-evolution-tree-*.png       # Game mechanics evolution
│   ├── knowledgebase-diagram.md                  # Diagram documentation
│   ├── family-tree.md                            # Family tree documentation
│   └── weapon-family-tree.md                     # Weapon family documentation
├── docs/
│   ├── README.md                  # Complete documentation overview
│   ├── predicate-reference.md     # Comprehensive predicate reference
│   ├── quick-reference.md         # Quick reference for common predicates
│   ├── recursive-rules.md         # Detailed recursive algorithm analysis
│   ├── technical-implementation.md # System architecture and implementation
│   └── usage-examples.md          # Examples with expected outputs
├── report/
│   ├── compile-report.bat         # Windows LaTeX compilation script
│   ├── compile-report.sh          # Unix LaTeX compilation script
│   ├── lab-report.tex             # LaTeX source for academic report
│   └── lab-report.pdf             # Compiled academic report (PDF)
├── scripts/
│   ├── demo.pl                    # Feature demonstration script
│   └── validate.pl                # Quick validation script
├── src/
│   └── weapons.pl                 # Main Prolog knowledgebase
├── tests/
│   └── weapon_tests.pl           # Comprehensive test queries
├── .gitignore                     # Git ignore patterns
├── LICENSE                        # MIT License with educational notice
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

### Comprehensive Visual Documentation

This project includes an extensive collection of **visual diagrams** that illustrate the complex relationships and structures within the knowledgebase:

- **5 major diagram categories** covering system architecture, classifications, and progressions
- **10+ individual diagrams** available in both horizontal and vertical orientations
- **Professional quality visualizations** suitable for academic presentations and reports
- **Complete documentation** explaining each diagram's purpose and content

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

For quick validation:

```prolog
?- consult('scripts/validate.pl').
```

For feature demonstration:

```prolog
?- consult('scripts/demo.pl').
```

## Documentation

### Complete Documentation Suite

The `docs/` folder contains comprehensive documentation covering all aspects of this knowledgebase:

#### 📖 [**Complete Documentation Overview**](docs/README.md)

- Project statistics and feature overview
- Navigation guide to all documentation
- Quick start examples and getting started guide
- Documentation standards and conventions

#### 🔍 [**Quick Reference**](docs/quick-reference.md)

- Concise reference for commonly used predicates
- Essential queries for everyday use
- Fast lookup for core functionality

#### 📚 [**Predicate Reference**](docs/predicate-reference.md)

- Complete listing of all 30+ predicates
- Detailed syntax, parameters, and return values
- Comprehensive examples for each predicate
- Organized by functional categories

#### 🔄 [**Recursive Rules**](docs/recursive-rules.md)

- Detailed explanation of recursive algorithms
- Implementation analysis and complexity
- Progression chain analysis
- Depth calculation and optimization techniques

#### ⚙️ [**Technical Implementation**](docs/technical-implementation.md)

- System architecture and design decisions
- Data modeling strategies
- Prolog best practices and conventions
- Performance considerations and optimizations

#### 💡 [**Usage Examples**](docs/usage-examples.md)

- Comprehensive examples with expected outputs
- Real-world use cases and scenarios
- Advanced query combinations
- Troubleshooting and common patterns

### Visual Documentation

The `diagrams/` folder contains comprehensive visual representations of the system:

#### System Architecture Diagrams

- **knowledgebase-diagram-horizontal.png**: Horizontal view of the complete system architecture
- **knowledgebase-diagram-vertical.png**: Vertical view of the complete system architecture

#### Classification and Hierarchy Diagrams

- **weapon-type-family-classification-horizontal.png**: Weapon type classification structure (horizontal)
- **weapon-type-family-classification-vertical.png**: Weapon type classification structure (vertical)

#### Progression and Evolution Diagrams

- **weapon-progression-family-tree-horizontal.png**: Weapon unlock progression chains (horizontal)
- **weapon-progression-family-tree-vertical.png**: Weapon unlock progression chains (vertical)
- **attachment-progression-family-tree-horizontal.png**: Attachment progression dependencies (horizontal)
- **attachment-progression-family-tree-vertical.png**: Attachment progression dependencies (vertical)
- **game-mechanics-evolution-tree-horizontal.png**: Game mechanics evolution across titles (horizontal)
- **game-mechanics-evolution-tree-vertical.png**: Game mechanics evolution across titles (vertical)

#### Documentation Files

- **knowledgebase-diagram.md**: Technical documentation for system architecture diagrams
- **family-tree.md**: Documentation for family tree and progression diagrams
- **weapon-family-tree.md**: Detailed weapon family classification documentation

All diagrams are available in both horizontal and vertical orientations for optimal viewing and presentation needs.

### Academic Report

The `report/` folder contains the formal project documentation and submission materials:

- **lab-report.tex**: Complete LaTeX source for the academic project report
- **lab-report.pdf**: Professionally compiled PDF report ready for submission
- **compile-report.bat**: Windows batch script for compiling the LaTeX report
- **compile-report.sh**: Unix shell script for compiling the LaTeX report

The academic report includes:

- Comprehensive project analysis and technical documentation
- All five system diagrams integrated into the document
- Sample input/output demonstrations with Prolog code examples
- Technical challenges and solutions discussion
- Educational value assessment and future work recommendations

To compile the report:

```bash
# On Windows
cd report
.\compile-report.bat

# On Unix/Linux/macOS
cd report
chmod +x compile-report.sh
./compile-report.sh
```

## Prolog Conventions

- Snake_case naming for predicates and variables
- Proper fact termination with periods
- Tail-recursive implementations where applicable
- Comprehensive commenting with `%` blocks
- Logical grouping of related predicates

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

### Educational Use

This project demonstrates advanced Prolog programming concepts and techniques. The Call of Duty game series and all related trademarks are property of their respective owners (Activision, Infinity Ward, Treyarch, etc.). This project is not affiliated with or endorsed by any of the aforementioned entities.

The weapon data, game mechanics, and attachment information are based on publicly available information about the Call of Duty video game series and are used here for demonstration of Prolog programming concepts.
