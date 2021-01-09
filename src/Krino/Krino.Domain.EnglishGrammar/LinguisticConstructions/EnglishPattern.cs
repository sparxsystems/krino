using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public static class EnglishPattern
    {
        public static Pattern Morpheme(BigInteger attributes) => Morpheme(null, attributes);

        public static Pattern Morpheme(string patternName, BigInteger attributes) => new Pattern(patternName)
        {
            MorphemeRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, attributes),
            LeftRule = MorphemeRule.Nothing,
            RightRule = MorphemeRule.Nothing,
        };

        public static Pattern O1_I => On_I("O1-I", 1);

        public static Pattern O2_I => On_I("O2-I", 2).SetRightFirst();
        public static Pattern O3_I => On_I("O3-I", 3).SetRightFirst();
        public static Pattern O4_I => On_I("O4-I", 4).SetRightFirst();
        public static Pattern O5_I => On_I("O5-I", 5).SetRightFirst();

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, rightAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, leftAttributes),
            };

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger notLeftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, leftAttributes, notLeftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, rightAttributes, notRightAttributes),
            };

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern GrammerCharacterTransference(string patternName, BigInteger morphemeAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, rightAttributes).SetOrder(1),
            };

        public static Pattern EpsilonAdPosition(string patternName, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, rightAttributes),
            };

        public static Pattern EpsilonAdPosition(string patternName, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern MorphematicAdPosition(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRuleMaker.Something, rightAttributes),
            };

        private static Pattern On_I(string patternName, int valencyPosition) => new Pattern(patternName)
        {
            MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(valencyPosition),
            LeftRule = EnglishMorphemeRule.O_Lexeme.SetInheritance(InheritanceRuleMaker.Epsilon_U),
            RightRule = EnglishMorphemeRule.I_Lexeme.SetInheritance(InheritanceRuleMaker.Epsilon_U),
        };
    }
}
