using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule to eveluate if something (e.g. adtree element) matches the pattern.
    /// </summary>
    [DebuggerDisplay("{MorphemeRule}")]
    public class PatternRule : IEquatable<PatternRule>
    {
        public static PatternRule Nothing => new PatternRule(MorphemeRule.Nothing, MaskRule.Nothing);


        public MorphemeRule MorphemeRule { get; private set; }
        public IRule<BigInteger> PatternAttributesRule { get; private set; }

        public PatternRule(MorphemeRule morphemeRule)
            : this(morphemeRule, MaskRule.Anything)
        {

        }

        public PatternRule(MorphemeRule morphemeRule, IRule<BigInteger> patternAttributesRule)
        {
            MorphemeRule = morphemeRule ?? throw new ArgumentNullException(nameof(morphemeRule));
            PatternAttributesRule = patternAttributesRule ?? throw new ArgumentNullException(nameof(patternAttributesRule));
        }

        /// <summary>
        /// Checks if the adtree matches the rule.
        /// </summary>
        /// <param name="morpheme"></param>
        /// <param name="patternAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(Morpheme morpheme, BigInteger patternAttributes)
        {
            // Note: do not use IAdtree as the input parameter because it would be the cyclicl dependency.

            bool isMatch = MorphemeRule.Evaluate(morpheme) && PatternAttributesRule.Evaluate(patternAttributes);
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
