using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    /// <summary>
    /// Defines the pattern which contains rules how adtrees can be connected to each other.
    /// </summary>
    /// <remarks>
    /// Inspite of the fact there are property setters this object shall stay immutable!!!
    /// </remarks>
    [DebuggerDisplay("{Name}")]
    public class Pattern : IEquatable<Pattern>
    {
        public static Pattern Morpheme(IAttributesModel attributesModel, BigInteger attributes, string description = null)
            => Morpheme(attributesModel, null, attributes, null, description);

        public static Pattern Morpheme(IAttributesModel attributesModel, string morph, BigInteger attributes, string description = null)
            => Morpheme(attributesModel, morph, attributes, null, description);

        public static Pattern Morpheme(IAttributesModel attributesModel, BigInteger attributes, string patternName, string description)
            => Morpheme(attributesModel, null, attributes, patternName, description);

        public static Pattern Morpheme(IAttributesModel attributesModel, string morph, BigInteger attributes, string patternName, string description)
            => new Pattern(patternName)
            {
                Description = description,
                // Note: attribute rule must be IValue<BigInteger> rule.
                UpRule = new MorphemeRule(attributesModel, string.IsNullOrEmpty(morph) ? MorphRules.Something : MorphRules.Is(morph), MaskRule.Is(attributes)),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            };

        public static Pattern MonoTransference(IAttributesModel attributesModel, string patternName, BigInteger morphemeAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = "Rule which changes grammar characters.",
                UpRule = MorphemeRule.Is(attributesModel, "", morphemeAttributes),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes, MorphematicAdPositionRules.Nothing),
            };

        public static Pattern PairTransference(IAttributesModel attributesModel, string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, "", morphemeAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes, MorphematicAdPositionRules.Nothing),
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes, MorphematicAdPositionRules.Nothing),
            };

        public static Pattern PairTransference(IAttributesModel attributesModel, string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger notLeftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, "", morphemeAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes, notLeftAttributes, MorphematicAdPositionRules.Nothing),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes, notRightAttributes, MorphematicAdPositionRules.Nothing),
            };

        public static Pattern PairTransference(IAttributesModel attributesModel, string patternName, string description, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, "", morphemeAttributes),
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern EpsilonAdPosition(IAttributesModel attributesModel, string patternName, string description, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Epsilon,
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Anything, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Anything, rightAttributes),
            };

        public static Pattern MorphematicAdPosition(IAttributesModel attributesModel, string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, MorphRules.Something, morphemeAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes),
            };
        public static Pattern MorphematicAdPosition(IAttributesModel attributesModel, string patternName, string description, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, MorphRules.Something, morphemeAttributes),
                LeftRule = leftRule,
                RightRule = rightRule,
            };


        public static Pattern O1_I(IAttributesModel attributesModel) => On_I(attributesModel, "O1-I", 1)
            .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing);
        public static Pattern O2_I(IAttributesModel attributesModel) => On_I(attributesModel, "O2-I", 2)
            .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing);
        public static Pattern O3_I(IAttributesModel attributesModel) => On_I(attributesModel, "O3-I", 3)
            .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing);
        public static Pattern O4_I(IAttributesModel attributesModel) => On_I(attributesModel, "O4-I", 4)
            .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing);
        public static Pattern O5_I(IAttributesModel attributesModel) => On_I(attributesModel, "O5-I", 5)
            .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing);

        // E.g. Speaking is prohibited. 'prohibited' is on the 2nd valency position.
        public static Pattern A2_I(IAttributesModel attributesModel) => An_I(attributesModel, "A2-I", 2)
            .SetMorphematicAdPositionRuleForRight(MorphematicAdPositionRules.Nothing);

        private static Pattern On_I(IAttributesModel attributesModel, string patternName, int valencyPosition)
            => new Pattern(patternName)
        {
            Description = $"Rule accepting stative lexeme on valency position {valencyPosition}.",
            ValencyPosition = valencyPosition,
            UpRule = MorphemeRule.Epsilon,
            LeftRule = MorphemeRule.O_Lexeme_Anything(attributesModel),
            RightRule = MorphemeRule.I_Lexeme_Anything(attributesModel),
        };

        private static Pattern An_I(IAttributesModel attributesModel, string patternName, int valencyPosition)
            => new Pattern(patternName)
        {
            Description = $"Rule accepting adjective lexeme on valency position {valencyPosition}.",
            ValencyPosition = valencyPosition,
            UpRule = MorphemeRule.Epsilon,
            LeftRule = MorphemeRule.A_Lexeme_Adjective_Anything(attributesModel),
            RightRule = MorphemeRule.I_Lexeme_Anything(attributesModel),
        };



        private string myName;

        public Pattern(string name = null)
        {
            myName = name;

            LeftPatternRule = PatternRules.ByLeftMorphemeRule(this);
            RightPatternRule = PatternRules.ByRightMorphemeRule(this);
        }

        public Pattern(Pattern pattern)
            : this(pattern.Name)
        {
            Description = pattern.Description;
            UpRule = new MorphemeRule(pattern.UpRule);
            LeftRule = new MorphemeRule(pattern.LeftRule);
            RightRule = new MorphemeRule(pattern.RightRule);
            ValencyPosition = pattern.ValencyPosition;
            IsLeftFirst = pattern.IsLeftFirst;

            LeftPatternRule = pattern.LeftPatternRule;
            RightPatternRule = pattern.RightPatternRule;
        }

        // Optional information for the debugging purposes.
        public string Name
        {
            get
            {
                if (!string.IsNullOrEmpty(myName))
                {
                    return myName;
                }

                string name;
                if ((LeftRule == null || LeftRule.Equals(MorphemeRule.Nothing)) &&
                    (RightRule == null || RightRule.Equals(MorphemeRule.Nothing)))
                {
                    name = UpRule.GrammarCharacter.ToString();
                }
                else
                {
                    name = string.Join("", LeftRule?.GrammarCharacter.ToString(), "-", UpRule.GrammarCharacter, "-", RightRule?.GrammarCharacter);
                }

                return name;
            }
        }

        public string Description { get; set; }


        public MorphemeRule UpRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule LeftRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule RightRule { get; set; } = MorphemeRule.Nothing;



        public IRule<Pattern> LeftPatternRule { get; set; }

        public IRule<Pattern> RightPatternRule { get; set; }


        public GrammarCharacter RulingGrammarCharacter => IsLikeMorpheme ? UpRule.GrammarCharacter : RightRule.GrammarCharacter;

        /// <summary>
        /// The valency position required by the pattern.
        /// </summary>
        /// <remarks>
        /// If 0 then the valency position is not required.
        /// </remarks>
        public int ValencyPosition { get; set; }

        /// <summary>
        /// Indicates if the left branch is before the right branch in the phrase sequence.
        /// </summary>
        public bool IsLeftFirst { get; set; }

        public Pattern SetLeftFirst()
        {
            IsLeftFirst = true;
            return this;
        }

        public Pattern SetMorphematicAdPositionRuleForLeft(IRule<GrammarCharacter> inheritanceRule)
        {
            LeftRule.SetMorphematicAdPositionRule(inheritanceRule);
            return this;
        }

        public Pattern SetMorphematicAdPositionRuleForRight(IRule<GrammarCharacter> inheritanceRule)
        {
            RightRule.SetMorphematicAdPositionRule(inheritanceRule);
            return this;
        }

        public bool IsLikeMorpheme => IsMorpheme || IsMonoTransference | IsPairTransference;

        /// <summary>
        /// Returns true if the pattern represents a morpheme.
        /// </summary>
        /// <returns></returns>
        public bool IsMorpheme
        {
            get
            {
                if (UpRule.GrammarCharacter != GrammarCharacter.e &&
                    (UpRule.MorphRule.Equals(MorphRules.Something) || UpRule.MorphRule is IValueRule<string> upMorphValueRule && !string.IsNullOrEmpty(upMorphValueRule.Value)) &&
                    UpRule.AttributesRule is IValueRule<BigInteger> &&
                    LeftRule.Equals(MorphemeRule.Nothing) &&
                    RightRule.Equals(MorphemeRule.Nothing))
                {
                    return true;
                }

                return false;
            }
        }

        /// <summary>
        /// Returns true if the pattern uses both positions (left and right) to set the grammar character. 
        /// </summary>
        /// <remarks>
        /// The adtree driven by the pair transference acts as morpheme.
        /// E.g. this pattern can be used to create noun from a verb by the -ing suffix.
        /// </remarks>
        /// <returns></returns>
        public bool IsPairTransference
        {
            get
            {
                // AdPosition
                if (UpRule.GrammarCharacter != GrammarCharacter.e &&
                    UpRule.GrammarCharacter != GrammarCharacter.U &&
                    UpRule.AttributesRule is IValueRule<BigInteger> &&
                    UpRule.MorphRule.Equals(MorphRules.EmptyString))
                {
                    // Left.
                    if (LeftRule.GrammarCharacter != GrammarCharacter.e)
                    {
                        // Right - inheriting site.
                        if (RightRule.GrammarCharacter != GrammarCharacter.e)
                        {
                            return true;
                        }
                    }
                }

                return false;
            }
        }

        /// <summary>
        /// Returs true if the pattern just changes the grammar character of the right position (the left position stays empty).
        /// </summary>
        /// <remarks>
        /// E.g. english nouns can be used as adjectives.
        /// </remarks>
        /// <returns></returns>
        public bool IsMonoTransference
        {
            get
            {
                // AdPosition
                if (UpRule.GrammarCharacter != GrammarCharacter.e &&
                    UpRule.GrammarCharacter != GrammarCharacter.U &&
                    UpRule.AttributesRule is IValueRule<BigInteger> &&
                    UpRule.MorphRule.Equals(MorphRules.EmptyString))
                {
                    // Left.
                    if (LeftRule.Equals(MorphemeRule.Nothing))
                    {
                        // Right - inheriting site.
                        if (RightRule.GrammarCharacter != GrammarCharacter.e)
                        {
                            return true;
                        }
                    }
                }

                return false;
            }
        }

        public bool IsEpsilonAdPosition()
        {
            bool result = UpRule.Equals(MorphemeRule.Epsilon) &&
                          !RightRule.Equals(MorphemeRule.Nothing) &&
                          !LeftRule.Equals(MorphemeRule.Nothing) &&
                          RightRule.MorphRule.Equals(MorphRules.Anything) && !RightRule.AttributesRule.Evaluate(0) &&
                          LeftRule.MorphRule.Equals(MorphRules.Anything) && !LeftRule.AttributesRule.Evaluate(0);

            return result;
        }

        /// <summary>
        /// Returns true if the pattern represents an adposition with a morpheme.
        /// </summary>
        /// <remarks>
        /// E.g. an adposition with the U grammar character containing a conjunction.
        /// Or an adposition with the E grammar character containing a preposition.
        /// </remarks>
        /// <returns></returns>
        public bool IsMorphematicAdPosition()
        {
            bool result = UpRule.GrammarCharacter != GrammarCharacter.e &&
                          UpRule.MorphRule.Equals(MorphRules.Something) &&
                          (!RightRule.Equals(MorphemeRule.Nothing) || !LeftRule.Equals(MorphemeRule.Nothing));

            return result;
        }


        public bool Equals(Pattern other) =>
            UpRule.Equals(other.UpRule) &&
            LeftRule.Equals(other.LeftRule) &&
            RightRule.Equals(other.RightRule) &&

            RightPatternRule.Equals(other.RightPatternRule) &&
            LeftPatternRule.Equals(other.LeftPatternRule) &&

            ValencyPosition == other.ValencyPosition;

        public override bool Equals(object obj) => obj is Pattern otherPattern && Equals(otherPattern);


        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ UpRule.GetHashCode();
            hash = (hash * 16777619) ^ LeftRule.GetHashCode();
            hash = (hash * 16777619) ^ RightRule.GetHashCode();

            hash = (hash * 16777619) ^ RightPatternRule.GetHashCode();
            hash = (hash * 16777619) ^ LeftPatternRule.GetHashCode();

            hash = (hash * 16777619) ^ ValencyPosition.GetHashCode();

            return hash;
        }

        public static bool operator ==(Pattern pattern1, Pattern pattern2) => Equals(pattern1, pattern2);
        public static bool operator !=(Pattern pattern1, Pattern pattern2) => !Equals(pattern1, pattern2);
    }
}
