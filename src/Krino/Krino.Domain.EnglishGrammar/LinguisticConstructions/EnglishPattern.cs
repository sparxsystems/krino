using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public static class EnglishPattern
    {
        private static EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        public static Pattern Morpheme(BigInteger attributes, string description = null)
            => Pattern.Morpheme(myAttributesModel, attributes, description);

        public static Pattern Morpheme(string morph, BigInteger attributes, string description = null)
            => Pattern.Morpheme(myAttributesModel, morph, attributes, description);

        public static Pattern Morpheme(BigInteger attributes, string patternName, string description)
            => Pattern.Morpheme(myAttributesModel, attributes, patternName, description);

        public static Pattern Morpheme(string morph, BigInteger attributes, string patternName, string description)
            => Pattern.Morpheme(myAttributesModel, morph, attributes, patternName, description);

        public static Pattern O1_I => Pattern.O1_I(myAttributesModel);

        public static Pattern O2_I => Pattern.O2_I(myAttributesModel);
        public static Pattern O3_I => Pattern.O3_I(myAttributesModel);
        public static Pattern O4_I => Pattern.O4_I(myAttributesModel);
        public static Pattern O5_I => Pattern.O5_I(myAttributesModel);

        // E.g. Speaking is prohibited. 'prohibited' is on the 2nd valency position.
        public static Pattern A2_I => Pattern.A2_I(myAttributesModel);

        public static Pattern PairTransference(string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => Pattern.PairTransference(myAttributesModel, patternName, description, morphemeAttributes, leftAttributes, rightAttributes);

        public static Pattern PairTransference(string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger notLeftAttributes, BigInteger rightAttributes, BigInteger notRightAttributes)
            => Pattern.PairTransference(myAttributesModel, patternName, description, morphemeAttributes, leftAttributes, notLeftAttributes, rightAttributes, notRightAttributes);

        public static Pattern PairTransference(string patternName, string description, BigInteger morphemeAttributes, MorphemeRule leftRule, MorphemeRule rightRule)
            => Pattern.PairTransference(myAttributesModel, patternName, description, morphemeAttributes, leftRule, rightRule);

        public static Pattern MonoTransference(string patternName, BigInteger morphemeAttributes, BigInteger rightAttributes)
            => Pattern.MonoTransference(myAttributesModel, patternName, morphemeAttributes, rightAttributes);

        public static Pattern EpsilonAdPosition(string patternName, string description, BigInteger leftAttributes, BigInteger rightAttributes)
            => Pattern.EpsilonAdPosition(myAttributesModel, patternName, description, leftAttributes, rightAttributes);


        public static Pattern MorphematicAdPosition(string patternName, string description, BigInteger morphemeAttributes, BigInteger leftAttributes, BigInteger rightAttributes)
            => new Pattern(patternName)
            {
                Description = description,
                UpRule = EnglishMorphemeRule.Is(MorphRules.Something, morphemeAttributes),
                LeftRule = EnglishMorphemeRule.Is(MorphRules.Something, leftAttributes),
                RightRule = EnglishMorphemeRule.Is(MorphRules.Something, rightAttributes),
            };
    }
}
