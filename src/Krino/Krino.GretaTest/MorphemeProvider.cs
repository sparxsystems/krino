using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.GretaTest
{
    public static class MorphemeProvider
    {
        public static List<Morpheme> Morphemes => new List<Morpheme>()
        {
            // Lexemes.
            new Morpheme("and"){ Attributes = Attributes.U.Lexeme.Conjunction },
            new Morpheme("climate") { Attributes = Attributes.O.Lexeme.Noun },
            new Morpheme("bad") { Attributes = Attributes.A.Lexeme.Adjective.Attributive },
            new Morpheme("emergency") { Attributes = Attributes.O.Lexeme.Noun },
            new Morpheme("good") { Attributes = Attributes.A.Lexeme.Adjective.Attributive },
            new Morpheme("have") { Attributes = Attributes.I.Lexeme.Verb.Bivalent },
            new Morpheme("i") { Attributes = Attributes.O.Lexeme.Pronoun.Subjective },
            new Morpheme("news") { Attributes = Attributes.O.Lexeme.Noun },
            new Morpheme("regarding") { Attributes = Attributes.E.Lexeme.Preposition },
            new Morpheme("some") { Attributes = Attributes.A.Lexeme.Determiner },
            new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle },

            // Non-lexemes.
            new Morpheme(".") { Attributes = Attributes.U.NonLexeme.PunctuationMark.Period },
        };


    }
}
