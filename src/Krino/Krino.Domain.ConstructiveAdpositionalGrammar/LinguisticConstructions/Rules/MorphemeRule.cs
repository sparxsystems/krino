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

        public IRule<GrammarCharacter> InheritanceRule { get; private set; } = InheritanceRules.Epsilon_U_E;

        /// <summary>
        /// Indicates the phrase order in the adtree.
        /// </summary>
        /// <remarks>
        /// If the Order property for the left rule is less than for the right rule then the left branch of adtree goes first.
        /// Note, this is related only to the sequence order how the phrase is in the adtree is interpreted.
        /// </remarks>
        public int Order { get; private set; } = int.MaxValue;

        public MorphemeRule SetOrder(int order)
        {
            Order = order;
            return this;
        }

        public MorphemeRule SetInheritance(IRule<GrammarCharacter> inheritanceRule)
        {
            InheritanceRule = inheritanceRule;
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
                          InheritanceRule.Equals(other.InheritanceRule);
            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphRule.GetHashCode();
            hash = (hash * 16777619) ^ GrammarCharacter.GetHashCode();
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();
            hash = (hash * 16777619) ^ InheritanceRule.GetHashCode();
            hash = (hash * 16777619) ^ Order.GetHashCode();

            return hash;
        }

    }
}
