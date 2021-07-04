using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules
{
    public static class EnglishMorphemeRule
    {
        private static EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        public static MorphemeRule Is(string morph, BigInteger attributes) => MorphemeRule.Is(myAttributesModel, MorphRules.Is(morph), attributes, 0);

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes) => MorphemeRule.Is(myAttributesModel, morphRule, attributes, 0);

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes, BigInteger notAttributes) =>
            MorphemeRule.Is(myAttributesModel, morphRule, attributes, notAttributes);
            

        public static MorphemeRule O_Lexeme_Something => MorphemeRule.O_Lexeme_Something(myAttributesModel);

        public static MorphemeRule I_Lexeme_Something => MorphemeRule.I_Lexeme_Something(myAttributesModel);

        public static MorphemeRule A_Lexeme_Something => MorphemeRule.A_Lexeme_Something(myAttributesModel);
        public static MorphemeRule A_Lexeme_Adjective_Something => MorphemeRule.A_Lexeme_Adjective_Something(myAttributesModel);

        public static MorphemeRule E_Lexeme_Something => MorphemeRule.E_Lexeme_Something(myAttributesModel);


        public static MorphemeRule U_Lexeme_Something => MorphemeRule.U_Lexeme_Something(myAttributesModel);
        public static MorphemeRule U_NonLexeme_Something => MorphemeRule.U_NonLexeme_Something(myAttributesModel);
    }
}
