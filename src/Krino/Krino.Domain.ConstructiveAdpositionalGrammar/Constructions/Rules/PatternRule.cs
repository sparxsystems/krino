using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule to eveluate if something (e.g. adtree element) matches the pattern.
    /// </summary>
    [DebuggerDisplay("{myMorphemeRule} && {myPatternAttributesRule}")]
    public class PatternRule : IEquatable<PatternRule>
    {
        public static PatternRule Anything = new PatternRule(MorphemeRule.Anything, Rule.Anything<BigInteger>());
        public static PatternRule Nothing = new PatternRule(MorphemeRule.Nothing, Rule.Nothing<BigInteger>());
        public static PatternRule EpsilonValency1 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.First));
        public static PatternRule EpsilonValency2 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Second));
        public static PatternRule EpsilonValency3 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Third));
        public static PatternRule EpsilonValency4 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Fourth));
        public static PatternRule EpsilonValency5 = new PatternRule(MorphemeRule.Epsilon, MaskRule.Is(PatternAttributes.ValencyPosition.Fifth));

        private MorphemeRule myMorphemeRule;
        private IRule<BigInteger> myPatternAttributesRule;

        public PatternRule(MorphemeRule morphemeRule) : this(morphemeRule, Rule.Anything<BigInteger>())
        {
        }

        public PatternRule(MorphemeRule morphemeRule, IRule<BigInteger> patternAttributesRule)
        {
            myMorphemeRule = morphemeRule ?? throw new ArgumentNullException(nameof(morphemeRule));
            myPatternAttributesRule = patternAttributesRule ?? throw new ArgumentNullException(nameof(patternAttributesRule));
        }

        /// <summary>
        /// Returns grammar characters which can be accepted by the rule.
        /// </summary>
        /// <returns></returns>
        public IEnumerable<GrammarCharacter> GetMatchingGrammarCharacters()
        {
            GrammarCharacter[] allGrammarCharacters = GrammarCharacterExt.GetValues();
            foreach (GrammarCharacter grammarCharacter in allGrammarCharacters)
            {
                if (IsMatch(grammarCharacter))
                {
                    yield return grammarCharacter;
                }
            }
        }

        /// <summary>
        /// Returns true if the morpheme rule accepts the provided grammar character.
        /// </summary>
        /// <param name="grammarCharacter"></param>
        /// <returns></returns>
        public bool IsMatch(GrammarCharacter grammarCharacter)
        {
            bool result = myMorphemeRule.IsMatch(grammarCharacter);
            return result;
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
            bool isMatch = myMorphemeRule.IsMatch(morph, morphemeAttributes) && myPatternAttributesRule.Evaluate(patternAttributes);
            return isMatch;
        }


        public bool Equals(PatternRule other)
        {
            bool result = myMorphemeRule.Equals(other.myMorphemeRule) &&
                          myPatternAttributesRule.Equals(other.myPatternAttributesRule);
            return result;
        }

        public override bool Equals(object obj) => obj is PatternRule rule && Equals(rule);

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ myMorphemeRule.GetHashCode();
            hash = (hash * 16777619) ^ myPatternAttributesRule.GetHashCode();

            return hash;
        }
    }
}
