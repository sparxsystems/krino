using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Defines the pattern which contains rules how adtrees can be connected to each other.
    /// </summary>
    [DebuggerDisplay("{LeftRule} <- {Name} -> {RightRule}")]
    public class Pattern : IEquatable<Pattern>
    {
        public Pattern(string name = null)
        {
            Name = name;
        }

        // Optional information for the debugging purposes.
        public string Name { get; private set; }


        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule LeftRule { get; set; } = MorphemeRule.Nothing;

        public MorphemeRule RightRule { get; set; } = MorphemeRule.Nothing;

        public bool IsPrimitiveTransference()
        {
            // AdPosition
            if (MorphemeRule.GrammarCharacter != GrammarCharacter.Epsilon &&
                MorphemeRule.AttributesRule is IReferenceValueRule<BigInteger> &&
                (MorphemeRule.MorphRule.Equals(MorphRuleMaker.Nothing) ||
                 MorphemeRule.MorphRule.Evaluate("")))
            {
                // Left.
                if (LeftRule.Equals(MorphemeRule.Nothing))
                {
                    // Right - inheriting site.
                    if (RightRule.GrammarCharacter != GrammarCharacter.Epsilon)
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
            if (MorphemeRule.GrammarCharacter == GrammarCharacter.Epsilon &&
                (MorphemeRule.MorphRule.Equals(MorphRuleMaker.Nothing) ||
                 MorphemeRule.MorphRule.Evaluate("")))
            {
                // Left.
                if (LeftRule.GrammarCharacter != GrammarCharacter.Epsilon)
                {
                    // Right.
                    if (RightRule.GrammarCharacter != GrammarCharacter.Epsilon)
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
                if (LeftRule.GrammarCharacter != GrammarCharacter.Epsilon)
                {
                    // Right.
                    if (RightRule.GrammarCharacter != GrammarCharacter.Epsilon)
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
