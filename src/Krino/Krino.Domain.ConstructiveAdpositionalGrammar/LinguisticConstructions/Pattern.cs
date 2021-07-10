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

        public static Pattern UnipolarMorphemeTransference(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, BigInteger rightAttributes)
        {
            var upGrammarCharacter = attributesModel.GetGrammarCharacter(upAttributes);
            if (upGrammarCharacter == GrammarCharacter.e)
            {
                throw new InvalidOperationException($"Failed to create {nameof(UnipolarMorphemeTransference)} because up grammar character was 'e'.");
            }
            var rightGrammarCharacter = attributesModel.GetGrammarCharacter(rightAttributes);
            if (rightGrammarCharacter == GrammarCharacter.e)
            {
                throw new InvalidOperationException($"Failed to create {nameof(UnipolarMorphemeTransference)} because right child grammar character was 'e'.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, "", upAttributes),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes),
            }
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));

            return result;
        }


        public static Pattern BipolarMorphemeTransference(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes,
            BigInteger leftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes = default)
        {
            var upGrammarCharacter = attributesModel.GetGrammarCharacter(upAttributes);
            if (upGrammarCharacter == GrammarCharacter.e || upGrammarCharacter == GrammarCharacter.U)
            {
                throw new InvalidOperationException($"Failed to create {nameof(BipolarMorphemeTransference)} because up grammar character was '{upGrammarCharacter}'. One of A, E, I is expected.");
            }
            if (!attributesModel.IsNonLexeme(leftAttributes))
            {
                throw new InvalidOperationException($"Failed to create {nameof(BipolarMorphemeTransference)} because left child is not NonLexeme.");
            }
            if (!attributesModel.IsLexeme(rightAttributes))
            {
                throw new InvalidOperationException($"Failed to create {nameof(BipolarMorphemeTransference)} because right child is not Lexeme.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, "", upAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes, notRightAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes),
            }
            .AndLeftPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing))
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Epsilon));

            return result;
        }


        public static Pattern BipolarMorphemeTransference(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, 
            string leftMorph, BigInteger leftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes = default)
        {
            var upGrammarCharacter = attributesModel.GetGrammarCharacter(upAttributes);
            if (upGrammarCharacter == GrammarCharacter.e || upGrammarCharacter == GrammarCharacter.U)
            {
                throw new InvalidOperationException($"Failed to create {nameof(BipolarMorphemeTransference)} because up grammar character was '{upGrammarCharacter}'. One of A, E, I is expected.");
            }
            if (!attributesModel.IsNonLexeme(leftAttributes))
            {
                throw new InvalidOperationException($"Failed to create {nameof(BipolarMorphemeTransference)} because left child is not NonLexeme.");
            }
            if (!attributesModel.IsLexeme(rightAttributes))
            {
                throw new InvalidOperationException($"Failed to create {nameof(BipolarMorphemeTransference)} because right child is not Lexeme.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, "", upAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, !string.IsNullOrEmpty(leftMorph) ? MorphRules.Is(leftMorph) : MorphRules.Something, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes, notRightAttributes),
            }
            .AndLeftPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing))
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Epsilon));

            return result;
        }
            

        public static Pattern MorphematicAdPosition(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => MorphematicAdPosition(patternName, description,
                MorphemeRule.Is(attributesModel, MorphRules.Something, upAttributes),
                MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes),
                MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes));

        public static Pattern MorphematicAdPosition(IAttributesModel attributesModel, string patternName, string description,
            string upMorph, BigInteger upAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => MorphematicAdPosition(patternName, description,
                MorphemeRule.Is(attributesModel, upMorph, upAttributes),
                MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes),
                MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes));

        public static Pattern MorphematicAdPosition(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => MorphematicAdPosition(patternName, description,
                MorphemeRule.Is(attributesModel, MorphRules.Something, upAttributes),
                leftRule,
                rightRule);

        public static Pattern MorphematicAdPosition(string patternName, string description,
            MorphemeRule upRule, MorphemeRule leftRule, MorphemeRule rightRule)
        {
            var upGrammarCharacter = upRule.GrammarCharacter;
            if (upRule.MorphRule.Equals(MorphRules.EmptyString))
            {
                // Note: this is how it differs from GrammaticalAdPosition.
                throw new InvalidOperationException($"Failed to create {nameof(MorphematicAdPosition)} because up-morpheme rule accepts empty string.");
            }
            if (upGrammarCharacter != GrammarCharacter.E && upGrammarCharacter != GrammarCharacter.U)
            {
                throw new InvalidOperationException($"Failed to create {nameof(MorphematicAdPosition)} because up-grammar character was '{upGrammarCharacter}'. But 'U' or 'E' is expected.");
            }
            if (upGrammarCharacter == GrammarCharacter.E && (leftRule == MorphemeRule.Nothing || rightRule == MorphemeRule.Nothing))
            {
                throw new InvalidOperationException($"Failed to create {nameof(MorphematicAdPosition)} because left or right child has 'MorphemeRule.Nothing'.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = upRule,
                LeftRule = leftRule,
                RightRule = rightRule,
            };
            result.UpPatternRule = PatternRules.ByUpMorphemeRule(result);

            return result;
        }

        public static Pattern GrammarAdPosition(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
        {
            var upGrammarCharacter = attributesModel.GetGrammarCharacter(upAttributes);
            if (upGrammarCharacter == GrammarCharacter.e)
            {
                throw new InvalidOperationException($"Failed to create {nameof(GrammarAdPosition)} because up-grammar character was 'e'.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, MorphRules.EmptyString, upAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Something, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes),
            };

            return result;
        }

        public static Pattern GrammarAdPosition(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, string leftMorph, BigInteger leftAttributes, BigInteger rightAttributes)
        {
            var upGrammarCharacter = attributesModel.GetGrammarCharacter(upAttributes);
            if (upGrammarCharacter == GrammarCharacter.e)
            {
                throw new InvalidOperationException($"Failed to create {nameof(GrammarAdPosition)} because up-grammar character was 'e'.");
            }
            if (string.IsNullOrEmpty(leftMorph))
            {
                throw new InvalidOperationException($"Failed to create {nameof(GrammarAdPosition)} because {nameof(leftMorph)} was set to null or empty string.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, MorphRules.EmptyString, upAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, leftMorph, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Something, rightAttributes),
            };

            return result;
        }

        public static Pattern GrammarAdPosition(IAttributesModel attributesModel, string patternName, string description,
            BigInteger upAttributes, string leftMorph, BigInteger leftAttributes, string rightMorph, BigInteger rightAttributes)
        {
            var upGrammarCharacter = attributesModel.GetGrammarCharacter(upAttributes);
            if (upGrammarCharacter == GrammarCharacter.e)
            {
                throw new InvalidOperationException($"Failed to create {nameof(GrammarAdPosition)} because up-grammar character was 'e'.");
            }
            if (string.IsNullOrEmpty(leftMorph))
            {
                throw new InvalidOperationException($"Failed to create {nameof(GrammarAdPosition)} because {nameof(leftMorph)} was set to null or empty string.");
            }
            if (string.IsNullOrEmpty(rightMorph))
            {
                throw new InvalidOperationException($"Failed to create {nameof(GrammarAdPosition)} because {nameof(rightMorph)} was set to null or empty string.");
            }

            var result = new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Is(attributesModel, MorphRules.EmptyString, upAttributes),
                LeftRule = MorphemeRule.Is(attributesModel, leftMorph, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, rightMorph, rightAttributes),

                UpPatternRule = PatternRules.Anything
            };

            return result;
        }

        public static Pattern EpsilonAdPosition(IAttributesModel attributesModel, string patternName, string description, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Epsilon,
                LeftRule = MorphemeRule.Is(attributesModel, MorphRules.Anything, leftAttributes),
                RightRule = MorphemeRule.Is(attributesModel, MorphRules.Anything, rightAttributes),
            };



        public static Pattern O1_I(IAttributesModel attributesModel) => On_I(attributesModel, "O1-I", 1)
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));
        public static Pattern O2_I(IAttributesModel attributesModel) => On_I(attributesModel, "O2-I", 2)
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));
        public static Pattern O3_I(IAttributesModel attributesModel) => On_I(attributesModel, "O3-I", 3)
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));
        public static Pattern O4_I(IAttributesModel attributesModel) => On_I(attributesModel, "O4-I", 4)
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));
        public static Pattern O5_I(IAttributesModel attributesModel) => On_I(attributesModel, "O5-I", 5)
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));

        // E.g. Speaking is prohibited. 'prohibited' is on the 2nd valency position.
        public static Pattern A2_I(IAttributesModel attributesModel) => An_I(attributesModel, "A2-I", 2)
            .AndRightPatternRule(PatternRules.MorphematicAdPosition(GrammarCharacterRules.Nothing));

        private static Pattern On_I(IAttributesModel attributesModel, string patternName, int valencyPosition)
            => new Pattern(patternName)
        {
            Description = $"Rule accepting stative lexeme on valency position {valencyPosition}.",
            ValencyPosition = valencyPosition,
            UpRule = MorphemeRule.Epsilon,
            LeftRule = MorphemeRule.O_Lexeme_Anything(attributesModel),
            RightRule = MorphemeRule.I_Lexeme_Verb_Anything(attributesModel),
        };

        private static Pattern An_I(IAttributesModel attributesModel, string patternName, int valencyPosition)
            => new Pattern(patternName)
        {
            Description = $"Rule accepting adjective lexeme on valency position {valencyPosition}.",
            ValencyPosition = valencyPosition,
            UpRule = MorphemeRule.Epsilon,
            LeftRule = MorphemeRule.A_Lexeme_Adjective_Anything(attributesModel),
            RightRule = MorphemeRule.I_Lexeme_Verb_Anything(attributesModel),
        };



        private string myName;

        public Pattern(string name = null)
        {
            myName = name;

            UpPatternRule = PatternRules.Nothing;
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

            UpPatternRule = PatternRules.Nothing;
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


        public IRule<Pattern> UpPatternRule { get; set; }


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

        public Pattern SetRightFirst()
        {
            IsLeftFirst = false;
            return this;
        }

        public Pattern AndLeftPatternRule(IRule<Pattern> ruleToAdd)
        {
            LeftPatternRule = LeftPatternRule.And(ruleToAdd);
            return this;
        }

        public Pattern AndRightPatternRule(IRule<Pattern> ruleToAdd)
        {
            RightPatternRule = RightPatternRule.And(ruleToAdd);
            return this;
        }

        public Pattern SetLeftPatternRule(IRule<Pattern> patternRule)
        {
            LeftPatternRule = patternRule;
            return this;
        }

        public Pattern SetRightPatternRule(IRule<Pattern> patternRule)
        {
            RightPatternRule = patternRule;
            return this;
        }

        public bool IsLikeMorpheme => IsMorpheme || IsUnipolarMorphemeTransference | IsBipolarMorphemeTransference;

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
        /// Returns true if the pattern transforms a lexeme on right without using the left.
        /// </summary>
        /// <remarks>
        /// E.g. english nouns can be used as adjectives.
        /// </remarks>
        /// <returns></returns>
        public bool IsUnipolarMorphemeTransference
        {
            get
            {
                // Up grammar character cannot be e and U.
                // Up morph rule must be empty string.
                // Up attributes rule must be a value - like a morpheme.
                // Left rule must attach nothing.
                // Right rule grammar character cannot be e.

                var result =
                    UpRule.GrammarCharacter != GrammarCharacter.e && UpRule.GrammarCharacter != GrammarCharacter.U &&
                    UpRule.AttributesRule is IValueRule<BigInteger> &&

                    // Note: operator == does not work here. I do not know why!
                    UpRule.MorphRule.Equals(MorphRules.EmptyString) &&
                    
                    LeftRule == MorphemeRule.Nothing &&
                    RightRule.GrammarCharacter != GrammarCharacter.e;

                return result;
            }
        }

        /// <summary>
        /// Returns true if the pattern uses a non-lexeme on the left to transform the lexem on right.
        /// </summary>
        /// <remarks>
        /// E.g. this pattern can be used to create noun from a verb by adding the -ing suffix.
        /// </remarks>
        /// <returns></returns>
        public bool IsBipolarMorphemeTransference
        {
            get
            {
                // Up grammar character cannot be e and U.
                // Up morph rule must be empty string.
                // Up attributes rule must be a value - like a morpheme.
                // Left rule accepts non-lexemes.
                // Right rule accepts lexemes.

                var result =
                    UpRule.GrammarCharacter != GrammarCharacter.e && UpRule.GrammarCharacter != GrammarCharacter.U &&
                    UpRule.AttributesRule is IValueRule<BigInteger> &&

                    // Note: operator == does not work here. I do not know why!
                    UpRule.MorphRule.Equals(MorphRules.EmptyString) &&

                    LeftRule.IsNonLexeme &&
                    RightRule.IsLexeme;

                return result;
            }
        }

        /// <summary>
        /// Returns true if the pattern represents an adposition with the grammar character U or E
        /// and has a morpheme (conjunction or preposition).
        /// </summary>
        /// <remarks>
        /// E.g. an adposition with the U grammar character containing a conjunction.
        /// Or an adposition with the E grammar character containing a preposition.
        /// </remarks>
        /// <returns></returns>
        public bool IsMorphematicAdPosition
        {
            get
            {
                // Up grammar character is U or E.
                // Up morph is a non-empty string.
                // If E then both (Left and Right) must attach something.
                // If U then at least one (Left or Right) must attach something.

                var result =
                    UpRule.GrammarCharacter == GrammarCharacter.E && LeftRule != MorphemeRule.Nothing && RightRule != MorphemeRule.Nothing ||
                    UpRule.GrammarCharacter == GrammarCharacter.U && (LeftRule != MorphemeRule.Nothing || RightRule != MorphemeRule.Nothing) &&
                    
                    // Note: operator == does not work here. I do not know why!
                    !UpRule.MorphRule.Equals(MorphRules.EmptyString);

                return result;
            }
        }

        /// <summary>
        /// Returns true if the pattern represents a grammatical construction of words and a new grammar character or attributes are set.
        /// </summary>
        /// <remarks>
        /// E.g. future tense of verbs ins sentence: I (will read) book.
        /// </remarks>
        public bool IsGrammarAdPosition
        {
            get
            {
                // Up grammar character is not e.
                // Up morph is the EMPTY string - this is a difference from the morphematic adposition.
                // Left and right must attach something.

                var result =
                    UpRule.GrammarCharacter != GrammarCharacter.e &&
                    // Note: operator == does not work here. I do not know why!
                    UpRule.MorphRule.Equals(MorphRules.EmptyString) &&

                    LeftRule != MorphemeRule.Nothing && RightRule != MorphemeRule.Nothing;

                return result;
            }
        }


        /// <summary>
        /// Returns true if the pattern represents an adposition with the grammar character e.
        /// </summary>
        /// <returns></returns>
        public bool IsEpsilonAdPosition
        {
            get
            {
                // Up grammar character is e.
                // Both (Left and Right) must attach something.

                var result = UpRule.GrammarCharacter == GrammarCharacter.e && LeftRule != MorphemeRule.Nothing && RightRule != MorphemeRule.Nothing;
                return result;
            }
        }


        public bool Equals(Pattern other) =>
            UpRule.Equals(other.UpRule) &&
            LeftRule.Equals(other.LeftRule) &&
            RightRule.Equals(other.RightRule) &&

            RightPatternRule.Equals(other.RightPatternRule) &&
            LeftPatternRule.Equals(other.LeftPatternRule) &&

            ValencyPosition == other.ValencyPosition &&
            IsLeftFirst == other.IsLeftFirst;

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

            hash = (hash * 16777619) ^ IsLeftFirst.GetHashCode();

            return hash;
        }

        public static bool operator ==(Pattern pattern1, Pattern pattern2) => Equals(pattern1, pattern2);
        public static bool operator !=(Pattern pattern1, Pattern pattern2) => !Equals(pattern1, pattern2);
    }
}
