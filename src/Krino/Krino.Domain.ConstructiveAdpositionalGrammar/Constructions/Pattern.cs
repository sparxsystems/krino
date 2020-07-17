using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Defines the pattern which contains rules how adtrees can be connected to each other.
    /// </summary>
    [DebuggerDisplay("{Name}")]
    public class Pattern : IEquatable<Pattern>
    {
        public static Pattern Morpheme(BigInteger attributes) => Morpheme(null, attributes);

        public static Pattern Morpheme(string patternName, BigInteger attributes) => new Pattern(patternName)
        {
            MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, attributes),
            LeftRule = MorphemeRule.Nothing,
            RightRule = MorphemeRule.Nothing,
        };

        public static Pattern O1_I => On_I("O1-I", 1);

        public static Pattern O2_I => On_I("O2-I", 2).SetRightFirst();
        public static Pattern O3_I => On_I("O3-I", 3).SetRightFirst();
        public static Pattern O4_I => On_I("O4-I", 4).SetRightFirst();
        public static Pattern O5_I => On_I("O5-I", 5).SetRightFirst();

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Is("", morphemeAttributes),
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, rightAttributes),
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, leftAttributes),
            };

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger notLeftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Is("", morphemeAttributes),
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, leftAttributes, notLeftAttributes),
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, rightAttributes, notRightAttributes),
            };

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Is("", morphemeAttributes),
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern PrimitiveTransference(string patternName, BigInteger morphemeAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Is("", morphemeAttributes),
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, rightAttributes).SetOrder(1),
            };

        public static Pattern EpsilonAdPosition(string patternName, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, leftAttributes),
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, rightAttributes),
            };

        public static Pattern EpsilonAdPosition(string patternName, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern MorphematicAdPosition(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Is(MorphRuleMaker.Something, morphemeAttributes),
                LeftRule = MorphemeRule.Is(MorphRuleMaker.Something, leftAttributes),
                RightRule = MorphemeRule.Is(MorphRuleMaker.Something, rightAttributes),
            };

        private static Pattern On_I(string patternName, int valencyPosition) => new Pattern(patternName)
        {
            MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(valencyPosition),
            LeftRule = MorphemeRule.O_Lexeme,
            RightRule = MorphemeRule.I_Lexeme,
        };



        private string myName;

        public Pattern(string name = null)
        {
            myName = name;
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
                    name = MorphemeRule.GrammarCharacter.ToString();
                }
                else
                {
                    name = string.Join("", LeftRule?.GrammarCharacter.ToString(), "-", MorphemeRule.GrammarCharacter, "-", RightRule?.GrammarCharacter);
                }

                return name;
            }
        }


        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule LeftRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule RightRule { get; set; } = MorphemeRule.Nothing;

        public Pattern SetLeftFirst()
        {
            LeftRule.SetOrder(1);
            RightRule.SetOrder(2);
            return this;
        }

        public Pattern SetRightFirst()
        {
            LeftRule.SetOrder(2);
            RightRule.SetOrder(1);
            return this;
        }

        public Pattern SetInheritanceForLeft(IRule<GrammarCharacter> inheritanceRule)
        {
            LeftRule.SetInheritance(inheritanceRule);
            return this;
        }

        public Pattern SetInheritanceForRight(IRule<GrammarCharacter> inheritanceRule)
        {
            RightRule.SetInheritance(inheritanceRule);
            return this;
        }


        public bool IsMorpheme()
        {
            if (MorphemeRule.GrammarCharacter != GrammarCharacter.e &&
                MorphemeRule.MorphRule.Equals(MorphRuleMaker.Something) &&
                LeftRule.Equals(MorphemeRule.Nothing) &&
                RightRule.Equals(MorphemeRule.Nothing))
            {
                return true;
            }

            return false;
        }

        public bool IsTransference()
        {
            // AdPosition
            if (MorphemeRule.GrammarCharacter != GrammarCharacter.e &&
                MorphemeRule.GrammarCharacter != GrammarCharacter.U &&
                MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> &&
                MorphemeRule.MorphRule.Equals(MorphRuleMaker.EmptyString))
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

        public bool IsPrimitiveTransference()
        {
            // AdPosition
            if (MorphemeRule.GrammarCharacter != GrammarCharacter.e &&
                MorphemeRule.GrammarCharacter != GrammarCharacter.U &&
                MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> &&
                MorphemeRule.MorphRule.Equals(MorphRuleMaker.EmptyString))
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

        public bool IsEpsilonAdPosition()
        {
            bool result = MorphemeRule.Equals(MorphemeRule.Epsilon) &&
                          !RightRule.Equals(MorphemeRule.Nothing) &&
                          !LeftRule.Equals(MorphemeRule.Nothing) &&
                          !RightRule.MorphRule.Evaluate(null) && !RightRule.MorphRule.Evaluate("") && !RightRule.AttributesRule.Evaluate(0) &&
                          !LeftRule.MorphRule.Evaluate(null) && !LeftRule.MorphRule.Evaluate("") && !LeftRule.AttributesRule.Evaluate(0);

            return result;
        }

        public bool IsMorphematicAdPosition()
        {
            bool result = MorphemeRule.MorphRule.Equals(MorphRuleMaker.Something) &&
                          !MorphemeRule.AttributesRule.Evaluate(0) &&
                          !RightRule.Equals(MorphemeRule.Nothing) &&
                          !LeftRule.Equals(MorphemeRule.Nothing) &&
                          !RightRule.MorphRule.Evaluate(null) && !RightRule.MorphRule.Evaluate("") && !RightRule.AttributesRule.Evaluate(0) &&
                          !LeftRule.MorphRule.Evaluate(null) && !LeftRule.MorphRule.Evaluate("") && !LeftRule.AttributesRule.Evaluate(0);

            return result;
        }


        public bool Equals(Pattern other) => MorphemeRule.Equals(other.MorphemeRule) && LeftRule.Equals(other.LeftRule) && RightRule.Equals(other.RightRule);

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphemeRule.GetHashCode();
            hash = (hash * 16777619) ^ LeftRule.GetHashCode();
            hash = (hash * 16777619) ^ RightRule.GetHashCode();

            return hash;
        }
    }
}
