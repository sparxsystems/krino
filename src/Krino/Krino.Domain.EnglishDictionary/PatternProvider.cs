using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using System.Collections.Generic;

namespace Krino.Domain.EnglishDictionary
{
    public static class PatternProvider
    {
        public static List<Pattern> Patterns = new List<Pattern>()
        {
            EnglishPattern.O_Lexeme,
            EnglishPattern.I_Lexeme,
            EnglishPattern.A_Lexeme,
            EnglishPattern.E_Lexeme_Adverb,

            EnglishPattern.O1_I.SetLeftFirst(),
            EnglishPattern.O1_I,
            EnglishPattern.O2_I,
            EnglishPattern.O3_I,
            //EnglishPattern.O4_I,
            //EnglishPattern.O5_I,

            // Adjective on the second valency position. E.g. Speaking is prohibited.
            EnglishPattern.A2_I,

            EnglishPattern.O_to_A,

            EnglishPattern.O_Suffix,
            EnglishPattern.I_Suffix,

            
            EnglishPattern.Not_I,

            // simple future
            EnglishPattern.Will_I,

            // present perfect
            EnglishPattern.Have_I,

            // Continuouse present perfect.
            EnglishPattern.Been_I_ing,

            // Continouse
            EnglishPattern.I_ing,

            // Making adjective from verb.
            EnglishPattern.I_to_A_ed,

            // Making noun from verb.
            EnglishPattern.I_to_O_ing,

            // Plural noun.
            EnglishPattern.O_s,

            
            // Adjuncrive before stative.
            EnglishPattern.A_O,

            // Circumstantial adverb after verb valencies.
            EnglishPattern.E_I,


            // Verbant circumstantial with a preposition.
            EnglishPattern.O_E_I,
            
            // E.g. speed of light
            EnglishPattern.O_E_O,


            // E.g. O and O
            EnglishPattern.O_U_O,

            // E.g. I as O ('the world' as 'people were saying')
            EnglishPattern.I_U_O,

            // Complex and compund sentences.
            EnglishPattern.I_U_I,


            EnglishPattern.I_Period_I,
        };
    }
}
