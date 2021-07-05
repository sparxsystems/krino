using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using System.Collections.Generic;

namespace Krino.Domain.EnglishDictionary
{
    public static class PatternProvider
    {
        public static List<Pattern> Patterns = new List<Pattern>()
        {
            EnglishPattern.O_Lexeme_Noun,
            EnglishPattern.O_Lexeme_Pronoun,
            EnglishPattern.I_Lexeme_Verb,
            EnglishPattern.A_Lexeme_Adjective,
            EnglishPattern.A_Lexeme_Determiner,
            EnglishPattern.E_Lexeme_Adverb,
            EnglishPattern.E_Lexeme_Preposition,
            EnglishPattern.U_Lexeme_Conjunction,
            EnglishPattern.U_NonLexeme_Punctuation,

            EnglishPattern.O_Suffix_s,

            EnglishPattern.I_Suffix_ed,
            EnglishPattern.I_Suffix_er,
            EnglishPattern.I_Suffix_ing,
            EnglishPattern.I_Suffix_s,

            EnglishPattern.O1_I.SetLeftFirst(),
            EnglishPattern.O1_I,
            EnglishPattern.O2_I,
            EnglishPattern.O3_I,
            //EnglishPattern.O4_I,
            //EnglishPattern.O5_I,

            // Adjective on the second valency position. E.g. Speaking is prohibited.
            EnglishPattern.A2_I,

            EnglishPattern.O_to_A,

            EnglishPattern.I_to_I_s,
            EnglishPattern.I_to_not_I,

            // simple future
            EnglishPattern.I_to_I_will,

            // present perfect
            EnglishPattern.I_to_I_have,

            // Continuouse present perfect.
            EnglishPattern.I_to_been_I_ing,

            // Continouse
            EnglishPattern.I_to_I_ing,

            // Making adjective from verb.
            EnglishPattern.I_to_A_ed,

            // Making noun from verb.
            EnglishPattern.I_to_O_ing,

            EnglishPattern.I_to_O_er,

            // Plural noun.
            EnglishPattern.O_to_O_s,

            
            // Adjuncrive before stative.
            EnglishPattern.A_O,

            // Circumstantial adverb after verb valencies.
            EnglishPattern.E_I,


            // Verbant circumstantial with a preposition.
            EnglishPattern.O_E_I,
            
            // E.g. speed of light
            EnglishPattern.O_E_O,

            EnglishPattern.A_U_A,

            // E.g. O and O
            EnglishPattern.O_U_O,

            // E.g. I as O ('the world' as 'people were saying')
            EnglishPattern.I_U_O,

            // Complex and compund sentences.
            EnglishPattern.I_U_I,


            EnglishPattern.e_Period_I,
        };
    }
}
