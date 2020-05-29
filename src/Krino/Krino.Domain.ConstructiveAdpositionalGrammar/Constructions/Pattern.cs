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
        private string myName;

        public Pattern(string name = null)
        {
            myName = name;
        }

        // Optional information for the debugging purposes.
        public string Name => myName ?? string.Join("", LeftRule?.GrammarCharacter.ToString(), "-", MorphemeRule.GrammarCharacter, "-", RightRule?.GrammarCharacter);


        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule LeftRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule RightRule { get; set; } = MorphemeRule.Nothing;


        public bool IsPrimitiveTransference()
        {
            // AdPosition
            if (MorphemeRule.GrammarCharacter != GrammarCharacter.e &&
                MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> &&
                (MorphemeRule.MorphRule.Equals(MorphRuleMaker.Something) ||
                 !MorphemeRule.MorphRule.Evaluate("") && !MorphemeRule.MorphRule.Evaluate(null)))
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

        public bool IsModifier()
        {
            // AdPosition
            if (MorphemeRule.GrammarCharacter == GrammarCharacter.e &&
                (MorphemeRule.MorphRule.Equals(MorphRuleMaker.Nothing) ||
                 MorphemeRule.MorphRule.Evaluate("")))
            {
                // Left.
                if (LeftRule.GrammarCharacter != GrammarCharacter.e)
                {
                    // Right.
                    if (RightRule.GrammarCharacter != GrammarCharacter.e)
                    {
                        if (RightRule.Order != LeftRule.Order)
                        {
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        public bool IsAdPositionModifier()
        {
            // AdPosition
            if (MorphemeRule.GrammarCharacter == GrammarCharacter.U &&
                !MorphemeRule.MorphRule.Equals(MorphRuleMaker.Nothing) &&
                !MorphemeRule.MorphRule.Evaluate(""))
            {
                // Left.
                if (LeftRule.GrammarCharacter != GrammarCharacter.e)
                {
                    // Right.
                    if (RightRule.GrammarCharacter != GrammarCharacter.e)
                    {
                        if (RightRule.Order != LeftRule.Order)
                        {
                            return true;
                        }
                    }
                }
            }

            return false;
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
