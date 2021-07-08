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
            EnglishPattern.I_Lexeme_Verb_Have,
            EnglishPattern.I_Lexeme_Verb_Been,
            EnglishPattern.I_Lexeme_Verb_Will,

            EnglishPattern.A_Lexeme_Adjective,
            EnglishPattern.A_Lexeme_Determiner,

            EnglishPattern.E_Lexeme_Adverb,
            EnglishPattern.E_Lexeme_Adverb_Not,
            EnglishPattern.E_Lexeme_Preposition,
            
            EnglishPattern.U_Lexeme_Conjunction,
            EnglishPattern.U_Lexeme_Conjunction_As,
            EnglishPattern.U_NonLexeme_Punctuation_Period,

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
            EnglishPattern.Not_I,

            // simple future
            EnglishPattern.Will_I,

            // present perfect
            EnglishPattern.Have_I,

            // Continuouse present perfect.
            EnglishPattern.Have_Been_Auxiliary,
            EnglishPattern.Have_Been_I_ing,

            // Continuouse
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
            EnglishPattern.I_as_O,

            // Complex and compund sentences.
            EnglishPattern.I_U_I,


            EnglishPattern.e_Period_I,
        };
    }
}
