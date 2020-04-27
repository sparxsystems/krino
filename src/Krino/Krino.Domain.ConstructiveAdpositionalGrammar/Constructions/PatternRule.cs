using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Rule to eveluate if something (e.g. adtree element) matches the pattern.
    /// </summary>
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

        /// <summary>
        /// Morpheme rule which needs to match in order to match this pattern rule.
        /// </summary>
        public MorphemeRule MorphemeRule { get; private set; }

        /// <summary>
        /// Attributes requiered to be present in order to match the pattern rule. (0 means any attributes will match.)
        /// </summary>
        public ulong RequiredPatternAttributes { get; private set; }

        /// <summary>
        /// Attributes which cannot be present in order to math the pattern rule. (0 means there are no rejected attributes.)
        /// </summary>
        public ulong RejectedPatternAttributes { get; private set; }

        /// <summary>
        /// Returns true if it matches the pattern rule.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="morphemeAttributes"></param>
        /// <param name="patternAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(string morph, ulong morphemeAttributes, ulong patternAttributes)
        {
            bool isMatch = !(RejectedPatternAttributes == ulong.MaxValue);

            if (isMatch)
            {
                isMatch = MorphemeRule.IsMatch(morph, morphemeAttributes);
                if (isMatch)
                {
                    isMatch = EnumBase.IsIn(RequiredPatternAttributes, patternAttributes);
                    if (isMatch && RejectedPatternAttributes != 0)
                    {
                        isMatch = !EnumBase.IsIn(RejectedPatternAttributes, patternAttributes);
                    }
                }
            }

            return isMatch;
        }

        public PatternRule Where(MorphemeRule morphemeRule) => new PatternRule(morphemeRule, RequiredPatternAttributes, RejectedPatternAttributes);

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
