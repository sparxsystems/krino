using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule to eveluate if something (e.g. adtree element) matches the pattern.
    /// </summary>
    [DebuggerDisplay("{MorphemeRule} && {PatternAttributesRule}")]
    public class PatternRule : IEquatable<PatternRule>
    {
        public static PatternRule Anything = new PatternRule(MorphemeRule.Anything, Rule.Anything<BigInteger>());
        public static PatternRule Nothing = new PatternRule(MorphemeRule.Nothing, Rule.Nothing<BigInteger>());
        public static PatternRule EpsilonValency1 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.First));
        public static PatternRule EpsilonValency2 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Second));
        public static PatternRule EpsilonValency3 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Third));
        public static PatternRule EpsilonValency4 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Fourth));
        public static PatternRule EpsilonValency5 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Fifth));

        public MorphemeRule MorphemeRule { get; private set; }
        public IRule<BigInteger> PatternAttributesRule { get; private set; }

        public PatternRule(MorphemeRule morphemeRule) : this(morphemeRule, Rule.Anything<BigInteger>())
        {
        }

        public PatternRule(MorphemeRule morphemeRule, IRule<BigInteger> patternAttributesRule)
        {
            MorphemeRule = morphemeRule ?? throw new ArgumentNullException(nameof(morphemeRule));
            PatternAttributesRule = patternAttributesRule ?? throw new ArgumentNullException(nameof(patternAttributesRule));
        }

        /// <summary>
        /// Returns true if it matches the pattern rule.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="morphemeAttributes"></param>
        /// <param name="patternAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(string morph, BigInteger morphemeAttributes, BigInteger patternAttributes)
        {
            bool isMatch = MorphemeRule.IsMatch(morph, morphemeAttributes) && PatternAttributesRule.Evaluate(patternAttributes);
            return isMatch;
        }


        public bool Equals(PatternRule other)
        {
            bool result = MorphemeRule.Equals(other.MorphemeRule) &&
                          PatternAttributesRule.Equals(other.PatternAttributesRule);
            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphemeRule.GetHashCode();
            hash = (hash * 16777619) ^ PatternAttributesRule.GetHashCode();

            return hash;
        }
    }
}
