using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    /// <summary>
    /// Rule evaluating morphemes.
    /// </summary>
    [DebuggerDisplay("{GrammarCharacter}: {MorphRule}")]
    public class MorphemeRule : IEquatable<MorphemeRule>
    {
        public static MorphemeRule Anything => new MorphemeRule(GrammarCharacter.e, MorphRules.Anything, MaskRule.Anything);

        public static MorphemeRule Nothing => new MorphemeRule(GrammarCharacter.e, MorphRules.Nothing, MaskRule.Nothing);

        public static MorphemeRule Epsilon => new MorphemeRule(GrammarCharacter.e, MorphRules.EmptyString, MaskRule.Is(0));



        public MorphemeRule(GrammarCharacter grammarCharacter, IRule<string> morphRule, IRule<BigInteger> attributesRule)
        {
            GrammarCharacter = grammarCharacter;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
        }


        /// <summary>
        /// Rule to evaluate the morph.
        /// </summary>
        public IRule<string> MorphRule { get; private set; }

        /// <summary>
        /// Grammar character accepted by the morpheme rule.
        /// </summary>
        public GrammarCharacter GrammarCharacter { get; private set; }

        /// <summary>
        /// Rule to evaluate morpheme attributes.
        /// </summary>
        public IRule<BigInteger> AttributesRule { get; private set; }

        /// <summary>
        /// Rule to evaluate if themorpheme can be provided via the substitution.
        /// </summary>
        /// <remarks>
        /// E.g. O can be provided via the A-O pattern.
        /// </remarks>
        public IRule<GrammarCharacter> SubstitutionRule { get; private set; } = InheritanceRules.Epsilon_U_E;

        public MorphemeRule SetSubstitution(IRule<GrammarCharacter> inheritanceRule)
        {
            SubstitutionRule = inheritanceRule;
            return this;
        }

        

        /// <summary>
        /// Checks if the morpheme matches the rule.
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        public bool Evaluate(Morpheme morpheme)
        {
            bool result = MorphRule.Evaluate(morpheme.Morph) && AttributesRule.Evaluate(morpheme.Attributes);
            return result;
        }

        public bool Equals(MorphemeRule other)
        {
            bool result = MorphRule.Equals(other.MorphRule) &&
                          GrammarCharacter == other.GrammarCharacter &&
                          AttributesRule.Equals(other.AttributesRule) &&
                          SubstitutionRule.Equals(other.SubstitutionRule);
            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphRule.GetHashCode();
            hash = (hash * 16777619) ^ GrammarCharacter.GetHashCode();
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();
            hash = (hash * 16777619) ^ SubstitutionRule.GetHashCode();

            return hash;
        }

    }
}
