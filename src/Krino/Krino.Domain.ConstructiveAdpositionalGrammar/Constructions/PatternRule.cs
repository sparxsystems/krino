using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public struct PatternRule
    {
        // Note: it is the struct to avoid incosistent situations if the PatternRule is null.

        public static PatternRule Anything = default;
        public static PatternRule Nothing = new PatternRule(MorphemeRule.Nothing, 0, ulong.MaxValue);
        public static PatternRule Epsilon = new PatternRule(MorphemeRule.Epsilon, 0, 0);
        public static PatternRule EpsilonValency1 = new PatternRule(MorphemeRule.Epsilon, PatternAttributes.ValencyPosition.First, 0);
        public static PatternRule EpsilonValency2 = new PatternRule(MorphemeRule.Epsilon, PatternAttributes.ValencyPosition.Second, 0);
        public static PatternRule EpsilonValency3 = new PatternRule(MorphemeRule.Epsilon, PatternAttributes.ValencyPosition.Third, 0);
        public static PatternRule EpsilonValency4 = new PatternRule(MorphemeRule.Epsilon, PatternAttributes.ValencyPosition.Fourth, 0);
        public static PatternRule EpsilonValency5 = new PatternRule(MorphemeRule.Epsilon, PatternAttributes.ValencyPosition.Fifth, 0);

        public PatternRule(MorphemeRule morphemeRule, ulong requiredPatternAttributes, ulong rejectedPatternAttributes)
        {
            MorphemeRule = morphemeRule;
            RequiredPatternAttributes = requiredPatternAttributes;
            RejectedPatternAttributes = rejectedPatternAttributes;
        }

        public MorphemeRule MorphemeRule { get; private set; }

        public ulong RequiredPatternAttributes { get; private set; }
        public ulong RejectedPatternAttributes { get; private set; }

        public bool IsMatch(string morph, ulong morphemeAttributes, ulong patternAttributes)
        {
            bool isMatch = MorphemeRule.IsMatch(morph, morphemeAttributes);
            if (isMatch)
            {
                isMatch = EnumBase.IsIn(RequiredPatternAttributes, patternAttributes);
                if (isMatch && RejectedPatternAttributes != 0)
                {
                    isMatch = !EnumBase.IsIn(RejectedPatternAttributes, patternAttributes);
                }
            }

            return isMatch;
        }


        public override bool Equals(object obj) => obj is PatternRule rule && this == rule;

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphemeRule.GetHashCode();
            hash = (hash * 16777619) ^ RequiredPatternAttributes.GetHashCode();
            hash = (hash * 16777619) ^ RejectedPatternAttributes.GetHashCode();

            return hash;
        }


        public static bool operator ==(PatternRule rule1, PatternRule rule2) =>
            rule1.MorphemeRule == rule2.MorphemeRule &&
            rule1.RequiredPatternAttributes == rule2.RequiredPatternAttributes &&
            rule1.RejectedPatternAttributes == rule2.RejectedPatternAttributes;

        public static bool operator !=(PatternRule rule1, PatternRule rule2) => !(rule1 == rule2);
    }
}
