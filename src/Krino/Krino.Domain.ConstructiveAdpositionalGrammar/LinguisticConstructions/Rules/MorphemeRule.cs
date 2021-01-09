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
        public static MorphemeRule Anything => new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.Anything, MaskRule.Anything);

        public static MorphemeRule Nothing => new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.Nothing, MaskRule.Nothing);

        public static MorphemeRule Epsilon => new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.EmptyString, MaskRule.Is(0));

        public MorphemeRule(GrammarCharacter grammarCharacter, IRule<string> morphRule, IRule<BigInteger> attributesRule)
        {
            GrammarCharacter = grammarCharacter;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
        }


        public IRule<string> MorphRule { get; private set; }
        public GrammarCharacter GrammarCharacter { get; private set; }
        public IRule<BigInteger> AttributesRule { get; private set; }

        public IRule<GrammarCharacter> InheritanceRule { get; private set; } = InheritanceRuleMaker.Epsilon_U_E;

        public int ValencyPosition { get; private set; }

        public int Order { get; private set; } = int.MaxValue;

        public MorphemeRule SetValencyPosition(int valencyPosition)
        {
            ValencyPosition = valencyPosition;
            return this;
        }

        public MorphemeRule SetOrder(int order)
        {
            Order = order;
            return this;
        }

        public MorphemeRule SetAttributes(BigInteger attributes)
        {
            AttributesRule = MaskRule.Is(attributes);
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
                          AttributesRule.Equals(other.AttributesRule);
            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphRule.GetHashCode();
            hash = (hash * 16777619) ^ GrammarCharacter.GetHashCode();
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();
            hash = (hash * 16777619) ^ ValencyPosition.GetHashCode();
            hash = (hash * 16777619) ^ Order.GetHashCode();

            return hash;
        }

    }
}
