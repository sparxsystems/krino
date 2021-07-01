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
        public static MorphemeRule Anything => new MorphemeRule(null, MorphRules.Anything, MaskRule.Anything);

        public static MorphemeRule Nothing => new MorphemeRule(null, MorphRules.Nothing, MaskRule.Nothing);

        public static MorphemeRule Epsilon => new MorphemeRule(null, MorphRules.EmptyString, MaskRule.Is(0));


        private IAttributesModel myAttributesModel;


        public MorphemeRule(IAttributesModel attributesModel, IRule<string> morphRule, IRule<BigInteger> attributesRule)
        {
            myAttributesModel = attributesModel;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
        }

        public MorphemeRule(MorphemeRule morphemeRule)
            : this(morphemeRule.myAttributesModel, morphemeRule.MorphRule, morphemeRule.AttributesRule)
        {
            SubstitutionRule = morphemeRule.SubstitutionRule;
        }


        /// <summary>
        /// Rule to evaluate the morph.
        /// </summary>
        public IRule<string> MorphRule { get; private set; }

        /// <summary>
        /// Grammar character accepted by the morpheme rule.
        /// </summary>
        public GrammarCharacter GrammarCharacter
        {
            get
            {
                GrammarCharacter result = GrammarCharacter.e;

                if (myAttributesModel != null && AttributesRule is IValueRule<BigInteger> valueRule)
                {
                    result = myAttributesModel.GetGrammarCharacter(valueRule.Value);
                }

                return result;
            }
        }

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
        public IRule<GrammarCharacter> SubstitutionRule { get; private set; } = SubstitutionRules.Epsilon_U_E;

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
                          AttributesRule.Equals(other.AttributesRule) &&
                          SubstitutionRule.Equals(other.SubstitutionRule);
            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphRule.GetHashCode();
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();
            hash = (hash * 16777619) ^ SubstitutionRule.GetHashCode();

            return hash;
        }

    }
}
