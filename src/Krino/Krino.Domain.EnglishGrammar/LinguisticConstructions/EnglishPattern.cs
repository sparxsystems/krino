using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public static class EnglishPattern
    {
        public static Pattern Morpheme(BigInteger attributes, string description = null) => Morpheme(attributes, null, description);

        public static Pattern Morpheme(BigInteger attributes, string patternName, string description) => new Pattern(patternName)
        {
            Description = description,
            UpRule = EnglishMorphemeRule.Is(MorphRules.Something, attributes),
            LeftRule = MorphemeRule.Nothing,
            RightRule = MorphemeRule.Nothing,
        };

        public static Pattern O1_I => On_I("O1-I", 1);

        public static Pattern O2_I => On_I("O2-I", 2);
        public static Pattern O3_I => On_I("O3-I", 3);
        public static Pattern O4_I => On_I("O4-I", 4);
        public static Pattern O5_I => On_I("O5-I", 5);

        // E.g. Speaking is prohibited. 'prohibited' is on the 2nd valency position.
        public static Pattern A2_I => An_I("A2-I", 2);

        public static Pattern PairTransference(string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
            };

        public static Pattern PairTransference(string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger notLeftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes, notLeftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes, notRightAttributes),
            };

        public static Pattern PairTransference(string patternName, string description, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern MonoTransference(string patternName, BigInteger morphemeAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = "Rule which changes grammar characters.",
                UpRule = EnglishMorphemeRule.Is("", morphemeAttributes),
                LeftRule = MorphemeRule.Nothing,
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
            };

        public static Pattern EpsilonAdPosition(string patternName, string description, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = MorphemeRule.Epsilon,
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
            };

        public static Pattern EpsilonAdPosition(string patternName, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                UpRule = MorphemeRule.Epsilon,
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        public static Pattern MorphematicAdPosition(string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = EnglishMorphemeRule.Is(MorphRules.Something, morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
            };

        public static Pattern MorphematicAdPosition(string patternName, MorphemeRule morphemeRule, MorphemeRule leftRule, MorphemeRule rightRule)
            => new Pattern(patternName)
            {
                UpRule = morphemeRule,
                LeftRule = leftRule,
                RightRule = rightRule,
            };

        private static Pattern On_I(string patternName, int valencyPosition) => new Pattern(patternName)
        {
            Description = $"Rule accepting stative lexeme on valency position {valencyPosition}.",
            ValencyPosition = valencyPosition,
            UpRule = MorphemeRule.Epsilon,
            LeftRule = EnglishMorphemeRule.O_Lexeme,
            RightRule = EnglishMorphemeRule.I_Lexeme,
        };

        private static Pattern An_I(string patternName, int valencyPosition) => new Pattern(patternName)
        {
            Description = $"Rule accepting adjective lexeme on valency position {valencyPosition}.",
            ValencyPosition = valencyPosition,
            UpRule = MorphemeRule.Epsilon,
            LeftRule = EnglishMorphemeRule.A_Lexeme_Adjective,
            RightRule = EnglishMorphemeRule.I_Lexeme,
        };
    }
}
