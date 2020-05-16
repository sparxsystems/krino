using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
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
        /// <summary>
        /// It does not accept any pattern.
        /// </summary>
        public static PatternRule Nothing => new PatternRule(MorphemeRule.Nothing, MaskRule.Nothing);

        public static PatternRule I => new PatternRule(MorphemeRule.I_Lexeme, MaskRule.Is(Attributes.I));

        public static PatternRule I_Lexeme => new PatternRule(MorphemeRule.I_Lexeme, MaskRule.Anything);

        public static PatternRule I_NonLexeme => new PatternRule(MorphemeRule.I_NonLexeme, MaskRule.Anything);




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
