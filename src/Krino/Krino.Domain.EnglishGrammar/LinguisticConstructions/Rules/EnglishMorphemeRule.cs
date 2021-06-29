using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
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
                new MorphemeRule(myAttributesModel.GetGrammarCharacter(attributes), morphRule, MaskRule.Is(attributes) & !MaskRule.Is(notAttributes)) :
                new MorphemeRule(myAttributesModel.GetGrammarCharacter(attributes), morphRule, MaskRule.Is(attributes));

        public static MorphemeRule O_Lexeme => new MorphemeRule(GrammarCharacter.O, MorphRules.Something, MaskRule.Is(EnglishAttributes.O.Lexeme));

        public static MorphemeRule O_NonLexeme => new MorphemeRule(GrammarCharacter.O, MorphRules.Something, MaskRule.Is(EnglishAttributes.O.NonLexeme));

        public static MorphemeRule I_Lexeme => new MorphemeRule(GrammarCharacter.I, MorphRules.Something, MaskRule.Is(EnglishAttributes.I.Lexeme));

        public static MorphemeRule A_Lexeme => new MorphemeRule(GrammarCharacter.A, MorphRules.Something, MaskRule.Is(EnglishAttributes.A.Lexeme));
        public static MorphemeRule A_Lexeme_Adjective => new MorphemeRule(GrammarCharacter.A, MorphRules.Something, MaskRule.Is(EnglishAttributes.A.Lexeme.Adjective));

        public static MorphemeRule E_Lexeme => new MorphemeRule(GrammarCharacter.E, MorphRules.Something, MaskRule.Is(EnglishAttributes.E.Lexeme));


        public static MorphemeRule U_Lexeme => new MorphemeRule(GrammarCharacter.U, MorphRules.Something, MaskRule.Is(EnglishAttributes.U.Lexeme));
        public static MorphemeRule U_NonLexeme => new MorphemeRule(GrammarCharacter.U, MorphRules.Something, MaskRule.Is(EnglishAttributes.U.NonLexeme));
    }
}
