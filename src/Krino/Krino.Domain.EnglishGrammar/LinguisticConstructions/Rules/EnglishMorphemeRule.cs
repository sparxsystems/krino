using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules
{
    public static class EnglishMorphemeRule
    {
        private static EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        public static MorphemeRule Is(string morph, BigInteger attributes) => Is(MorphRules.Is(morph), attributes, 0);

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes) => Is(morphRule, attributes, 0);

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes, BigInteger notAttributes) =>
            notAttributes != 0 ?
                new MorphemeRule(myAttributesModel, morphRule, MaskRule.Is(attributes) & !MaskRule.Is(notAttributes)) :
                new MorphemeRule(myAttributesModel, morphRule, MaskRule.Is(attributes));

        public static MorphemeRule O_Lexeme_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.O.Lexeme));

        public static MorphemeRule O_Lexeme_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.O.Lexeme));


        public static MorphemeRule I_Lexeme_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.I.Lexeme));

        public static MorphemeRule I_Lexeme_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.I.Lexeme));

        public static MorphemeRule A_Lexeme_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.A.Lexeme));
        public static MorphemeRule A_Lexeme_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.A.Lexeme));
        public static MorphemeRule A_Lexeme_Adjective_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.A.Lexeme.Adjective));
        public static MorphemeRule A_Lexeme_Adjective_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.A.Lexeme.Adjective));

        public static MorphemeRule E_Lexeme_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.E.Lexeme));
        public static MorphemeRule E_Lexeme_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.E.Lexeme));


        public static MorphemeRule U_Lexeme_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.U.Lexeme));
        public static MorphemeRule U_Lexeme_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.U.Lexeme));
        public static MorphemeRule U_NonLexeme_Something => new MorphemeRule(myAttributesModel, MorphRules.Something, MaskRule.Is(EnglishAttributes.U.NonLexeme));
        public static MorphemeRule U_NonLexeme_Anything => new MorphemeRule(myAttributesModel, MorphRules.Anything, MaskRule.Is(EnglishAttributes.U.NonLexeme));
    }
}
