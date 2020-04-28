using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule to eveluate if something (e.g. adtree element) matches the pattern.
    /// </summary>
    public class PatternRule
    {
        // Note: it is the struct to avoid incosistent situations if the PatternRule is null.

        public static PatternRule Anything = new PatternRule(MorphemeRule.Anything, Rule.Anything<ulong>());
        public static PatternRule Nothing = new PatternRule(MorphemeRule.Nothing, Rule.Nothing<ulong>());
        public static PatternRule Epsilon = new PatternRule(MorphemeRule.Epsilon, Rule.Anything<ulong>());
        public static PatternRule EpsilonValency1 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.First));
        public static PatternRule EpsilonValency2 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Second));
        public static PatternRule EpsilonValency3 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Third));
        public static PatternRule EpsilonValency4 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Fourth));
        public static PatternRule EpsilonValency5 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Fifth));

        private MorphemeRule myMorphemeRule;
        private IRule<ulong> myPatternAttributesRule;

        public PatternRule(MorphemeRule morphemeRule, IRule<ulong> patternAttributesRule)
        {
            myMorphemeRule = morphemeRule;
            myPatternAttributesRule = patternAttributesRule;
        }


        /// <summary>
        /// Returns true if it matches the pattern rule.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="morphemeAttributes"></param>
        /// <param name="patternAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(string morph, ulong morphemeAttributes, ulong patternAttributes)
        {
            bool isMatch = myMorphemeRule.IsMatch(morph, morphemeAttributes) && myPatternAttributesRule.Evaluate(patternAttributes);
            return isMatch;
        }

        //public PatternRule Where(MorphemeRule morphemeRule) => new PatternRule(morphemeRule, RequiredPatternAttributes, RejectedPatternAttributes);

        public override bool Equals(object obj) => obj is PatternRule rule && this == rule;

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ myMorphemeRule.GetHashCode();
            hash = (hash * 16777619) ^ myPatternAttributesRule.GetHashCode();

            return hash;
        }

        public static bool operator ==(PatternRule rule1, PatternRule rule2) =>
            rule1.myMorphemeRule == rule2.myMorphemeRule &&
            rule1.myPatternAttributesRule == rule2.myPatternAttributesRule;

        public static bool operator !=(PatternRule rule1, PatternRule rule2) => !(rule1 == rule2);
    }
}
