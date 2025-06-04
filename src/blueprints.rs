use crate::*;

#[derive(Resource)]
pub struct Blueprints {
    pub units: Vec<Unit>,
}

impl Blueprints {
    pub fn construct() -> Self {
        // archetypes
        // - weak units
        // - just attack
        // - buffing
        // - healing
        // - self damage
        // - energy gain infinite -> careful, should be hard to assemble
        // - converters life -> attack, energy -> life, ...
        // - area of effect
        // - being targeted, responses
        // - drain the team
        // - counterattack
        // - lifelink
        // - high cost high damage

        // shorthands (kinda macros)
        let life = || UnitValues::Life;
        let attack = || UnitValues::Attack;
        let energy = || UnitValues::Energy;

        let imm = |amt| CombatNumberSource::Immediate(amt);
        let src_unit = |v| CombatNumberSource::Unit(v);
        let src_unit_neg = |v| CombatNumberSource::UnitNegated(v);

        let num = |s, v| CombatNumber {
            source: s,
            values: v,
        };

        let fx = |target, gains| CombatEffect { target, gains };

        // effects shorthands
        let damage_mostlife_withattack = CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Life),
            gains: vec![num(src_unit_neg(attack()), life())],
        };
        let damage_mostenergy_withattack = CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Energy),
            gains: vec![num(src_unit_neg(attack()), life())],
        };
        let gain_self = |buffs: Vec<CombatNumber>| CombatEffect {
            target: CombatTarget::This,
            gains: buffs,
        };
        let gain_enemy_life = |amt| CombatEffect {
            target: CombatTarget::AllEnemy,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Life,
            }],
        };
        let gain_ally_life = |amt| CombatEffect {
            target: CombatTarget::AllAlly,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Life,
            }],
        };
        let gain_ally_energy = |amt| CombatEffect {
            target: CombatTarget::AllAlly,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Energy,
            }],
        };
        let gain_abilitysource_attack = |amt| CombatEffect {
            target: CombatTarget::AbilityTarget,
            gains: vec![num(imm(amt), attack())],
        };
        let gain_abilitysource_attack = |amt| CombatEffect {
            target: CombatTarget::AbilityTarget,
            gains: vec![num(imm(amt), attack())],
        };
        let gain_ally_attack = |amt| CombatEffect {
            target: CombatTarget::AllAlly,
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Attack,
            }],
        };
        let damage_mostlife_imm = |amt| CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Life),
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Life,
            }],
        };
        let drain_mostattack_imm = |amt| CombatEffect {
            target: CombatTarget::EnemyWithMost(UnitValues::Attack),
            gains: vec![CombatNumber {
                source: CombatNumberSource::Immediate(amt),
                values: UnitValues::Energy,
            }],
        };

        // trigger shorthands
        let on_other_loss = |v| CombatTrigger {
            target: CombatTarget::AllOther,
            watch: CombatTriggerWatch::ValueDecrease(v),
        };
        let on_otherally_lifeloss = CombatTrigger {
            target: CombatTarget::AllOtherAlly,
            watch: CombatTriggerWatch::ValueDecrease(UnitValues::Life),
        };
        let on_self_lifeloss = CombatTrigger {
            target: CombatTarget::This,
            watch: CombatTriggerWatch::ValueDecrease(UnitValues::Life),
        };
        let on_self_attackloss = CombatTrigger {
            target: CombatTarget::This,
            watch: CombatTriggerWatch::ValueDecrease(UnitValues::Attack),
        };

        // crazy units
        let units = vec![
            Unit {
                name: format!("Lucilla"),
                sprite_index: 2,
                level: 1,
                experience: 0,
                life: UnitValue::full(24),
                attack: UnitValue::full(5),
                energy: UnitValue::full(7),
                owner: Owner::Player,
                abilities: vec![CombatAbility {
                    effects: vec![damage_mostlife_withattack.clone()],
                    costs: vec![num(imm(5), energy())],
                    trigger: None,
                }],
            },
            Unit {
                name: format!("Ramona"),
                sprite_index: 6,
                level: 1,
                experience: 0,
                life: UnitValue::full(18),
                attack: UnitValue::full(1),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_ally_life(1)],
                        costs: vec![num(imm(10), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![damage_mostlife_withattack.clone()],
                        costs: vec![num(imm(5), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Sparks"),
                sprite_index: 0,
                level: 1,
                experience: 0,
                life: UnitValue::full(15),
                attack: UnitValue::full(1),
                energy: UnitValue::full(16),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![damage_mostlife_imm(-2)],
                    costs: vec![num(imm(5), energy())],
                    trigger: None,
                }],
            },
            Unit {
                name: format!("Thaunos"),
                sprite_index: 11,
                level: 1,
                experience: 0,
                life: UnitValue::full(30),
                attack: UnitValue::full(2),
                energy: UnitValue::full(6),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_self(vec![num(imm(2), attack())])],
                        costs: vec![num(imm(3), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![damage_mostlife_withattack.clone()],
                        costs: vec![num(imm(3), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Piston"),
                sprite_index: 11,
                level: 1,
                experience: 0,
                life: UnitValue::full(20),
                attack: UnitValue::full(2),
                energy: UnitValue::full(20),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_ally_energy(1)],
                        costs: vec![num(imm(16), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![drain_mostattack_imm(-2)],
                        costs: vec![num(imm(3), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Dorm"),
                sprite_index: 7,
                level: 1,
                experience: 0,
                life: UnitValue::full(20),
                attack: UnitValue::full(2),
                energy: UnitValue::full(20),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![gain_abilitysource_attack(2)],
                    costs: vec![num(imm(5), energy())],
                    trigger: Some(on_otherally_lifeloss.clone()),
                }],
            },
            Unit {
                name: format!("Candle"),
                sprite_index: 12,
                level: 1,
                experience: 0,
                life: UnitValue::full(7),
                attack: UnitValue::full(1),
                energy: UnitValue::full(12),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_self(vec![num(imm(-3), life())]), gain_enemy_life(-1)],
                        costs: vec![num(imm(6), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![
                            fx(CombatTarget::AllOtherAlly, vec![num(imm(2), energy())]),
                            fx(
                                CombatTarget::EnemyWithMost(energy()),
                                vec![num(imm(-4), energy())],
                            ),
                        ],
                        costs: vec![num(imm(6), energy())],
                        trigger: Some(on_self_lifeloss.clone()),
                    },
                ],
            },
            Unit {
                name: format!("Carmine"),
                sprite_index: 8,
                level: 1,
                experience: 0,
                life: UnitValue::full(12),
                attack: UnitValue::full(1),
                energy: UnitValue::full(24),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![gain_ally_attack(1)],
                        costs: vec![num(imm(10), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![gain_self(vec![num(imm(-10), life())]), gain_ally_attack(4)],
                        costs: vec![num(imm(10), energy())],
                        trigger: Some(on_self_attackloss.clone()),
                    },
                ],
            },
            Unit {
                name: format!("Walter"),
                sprite_index: 1,
                level: 1,
                experience: 0,
                life: UnitValue::full(24),
                attack: UnitValue::full(5),
                energy: UnitValue::full(3),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![damage_mostenergy_withattack.clone()],
                    costs: vec![num(imm(5), energy())],
                    trigger: None,
                }],
            },
            Unit {
                name: format!("Gray Eminence"),
                sprite_index: 10,
                level: 1,
                experience: 0,
                life: UnitValue::full(3),
                attack: UnitValue::full(1),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AbilityTarget, vec![num(imm(-1), life())])],
                        costs: vec![num(imm(1), energy())],
                        trigger: Some(on_other_loss(attack())),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(attack()),
                            vec![num(imm(-1), attack())],
                        )],
                        costs: vec![num(imm(5), energy())],
                        trigger: None,
                    },
                ],
            },
            Unit {
                name: format!("Gorb"),
                sprite_index: 4,
                level: 1,
                experience: 0,
                life: UnitValue::full(17),
                attack: UnitValue::full(3),
                energy: UnitValue::full(8),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllAlly, vec![num(imm(-1), life())])],
                        costs: vec![num(imm(5), energy())],
                        trigger: Some(on_other_loss(attack())),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(attack()),
                            vec![num(imm(-2), attack()), num(imm(-1), life())],
                        )],
                        costs: vec![num(imm(1), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOtherAlly,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                ],
            },
        ];

        let units = vec![
            // basic attack to damage
            Unit {
                name: format!("Spark"),
                sprite_index: 0,
                level: 1,
                experience: 0,
                life: UnitValue::full(10),
                attack: UnitValue::full(4),
                energy: UnitValue::full(4),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(1), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-2), life())],
                        )],
                        costs: vec![num(imm(1), attack())],
                        trigger: None,
                    },
                ],
            },
            // heal 1 aoe
            Unit {
                name: format!("Elena"),
                sprite_index: 6,
                level: 1,
                experience: 0,
                life: UnitValue::full(12),
                attack: UnitValue::full(0),
                energy: UnitValue::full(4),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![fx(CombatTarget::AllAlly, vec![num(imm(1), life())])],
                    costs: vec![num(imm(2), energy())],
                    trigger: None,
                }],
            },
            // enemy lifeloss is attack, good damager
            Unit {
                name: format!("Jade"),
                sprite_index: 2,
                level: 1,
                experience: 0,
                life: UnitValue::full(8),
                attack: UnitValue::full(0),
                energy: UnitValue::full(0),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-2), life())],
                        )],
                        costs: vec![num(imm(1), attack()), num(imm(1), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), attack())])],
                        costs: vec![],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllEnemy,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                ],
            },
            // ally lifeloss is 1 atk to them, bad atk to damage
            Unit {
                name: format!("Kramer"),
                sprite_index: 3,
                level: 1,
                experience: 0,
                life: UnitValue::full(18),
                attack: UnitValue::full(4),
                energy: UnitValue::full(0),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AbilityTarget, vec![num(imm(1), attack())])],
                        costs: vec![],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllAlly,
                            watch: CombatTriggerWatch::ValueDecrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(energy()),
                            vec![num(imm(-1), life())],
                        )],
                        costs: vec![num(imm(2), attack())],
                        trigger: None,
                    },
                ],
            },
            // ally lifegain is 1 atk to them, bad healer
            Unit {
                name: format!("Poppy"),
                sprite_index: 5,
                level: 1,
                experience: 0,
                life: UnitValue::full(12),
                attack: UnitValue::full(4),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AbilityTarget, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(3), energy())],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllOtherAlly,
                            watch: CombatTriggerWatch::ValueIncrease(life()),
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AllyWithLeast(life()),
                            vec![num(imm(3), life())],
                        )],
                        costs: vec![num(imm(5), energy())],
                        trigger: None,
                    },
                ],
            },
            // ally gainattack is scaling damage
            Unit {
                name: format!("Roth"),
                sprite_index: 7,
                level: 1,
                experience: 0,
                life: UnitValue::full(8),
                attack: UnitValue::full(2),
                energy: UnitValue::full(8),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![fx(
                        CombatTarget::EnemyWithMost(life()),
                        vec![num(src_unit_neg(attack()), life())],
                    )],
                    costs: vec![num(imm(4), energy())],
                    trigger: Some(CombatTrigger {
                        target: CombatTarget::AllAlly,
                        watch: CombatTriggerWatch::ValueIncrease(attack()),
                    }),
                }],
            },
            // ally gainlife is scaling damage
            Unit {
                name: format!("Scarlet"),
                sprite_index: 8,
                level: 1,
                experience: 0,
                life: UnitValue::full(5),
                attack: UnitValue::full(5),
                energy: UnitValue::full(8),
                owner: Owner::Enemy,
                abilities: vec![CombatAbility {
                    effects: vec![
                        fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(src_unit_neg(attack()), life())],
                        ),
                        fx(CombatTarget::AbilityTarget, vec![num(imm(-1), life())]),
                    ],
                    costs: vec![num(imm(4), energy())],
                    trigger: Some(CombatTrigger {
                        target: CombatTarget::AllAlly,
                        watch: CombatTriggerWatch::ValueIncrease(life()),
                    }),
                }],
            },
            // bloodletting for attack
            Unit {
                name: format!("Preston"),
                sprite_index: 14,
                level: 1,
                experience: 0,
                life: UnitValue::full(10),
                attack: UnitValue::full(0),
                energy: UnitValue::full(10),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllOther, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(2), life()), num(imm(6), energy())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(life()),
                            vec![num(imm(-2), life())],
                        )],
                        costs: vec![num(imm(3), attack())],
                        trigger: None,
                    },
                ],
            },
            // bloodletting for energy, energy drainer
            Unit {
                name: format!("Luna"),
                sprite_index: 13,
                level: 1,
                experience: 0,
                life: UnitValue::full(10),
                attack: UnitValue::full(0),
                energy: UnitValue::full(4),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::AllyWithMost(energy()),
                            vec![num(imm(1), energy())],
                        )],
                        costs: vec![num(imm(4), life())],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![fx(
                            CombatTarget::EnemyWithMost(energy()),
                            vec![num(imm(-2), energy())],
                        )],
                        costs: vec![num(imm(4), energy())],
                        trigger: None,
                    },
                ],
            },
            // attack on enemy targeted, gives back to the team
            Unit {
                name: format!("Thelonius"),
                sprite_index: 10,
                level: 1,
                experience: 0,
                life: UnitValue::full(4),
                attack: UnitValue::full(0),
                energy: UnitValue::full(0),
                owner: Owner::Enemy,
                abilities: vec![
                    CombatAbility {
                        effects: vec![fx(CombatTarget::This, vec![num(imm(1), attack())])],
                        costs: vec![],
                        trigger: Some(CombatTrigger {
                            target: CombatTarget::AllEnemy,
                            watch: CombatTriggerWatch::Targeted,
                        }),
                    },
                    CombatAbility {
                        effects: vec![fx(CombatTarget::AllOtherAlly, vec![num(imm(1), attack())])],
                        costs: vec![num(imm(3), attack())],
                        trigger: None,
                    },
                ],
            },
        ];

        Blueprints { units }
    }
}
