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
            MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, attributes),
            LeftRule = MorphemeRule.Nothing,
            RightRule = MorphemeRule.Nothing,
        };

        public static Pattern O1_I => On_I("O1-I", 1);

        public static Pattern O2_I => On_I("O2-I", 2);
        public static Pattern O3_I => On_I("O3-I", 3);
        public static Pattern O4_I => On_I("O4-I", 4);
        public static Pattern O5_I => On_I("O5-I", 5);

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
            };

        public static Pattern Transference(string patternName, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger notLeftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes, notLeftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes, notRightAttributes),
            };

        public static Pattern PairTransference(string patternName, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern MonoTransference(string patternName, BigInteger morphemeAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
            };

        public static Pattern EpsilonAdPosition(string patternName, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                MorphemeRule = MorphemeRule.Epsilon,
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
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
                MorphemeRule = EnglishMorphemeRule.Is(MorphRules.Something, morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
            };

        public static Pattern MorphematicAdPosition(string patternName, MorphemeRule morphemeRule, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                MorphemeRule = morphemeRule,
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        private static Pattern On_I(string patternName, int valencyPosition) => new Pattern(patternName)
        {
            ValencyPosition = valencyPosition,
            MorphemeRule = MorphemeRule.Epsilon,
            LeftRule = EnglishMorphemeRule.O_Lexeme.SetSubstitution(InheritanceRules.Epsilon_U),
            RightRule = EnglishMorphemeRule.I_Lexeme.SetSubstitution(InheritanceRules.Epsilon_U),
        };
    }
}
