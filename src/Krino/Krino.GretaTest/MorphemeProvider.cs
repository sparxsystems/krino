using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using System.Collections.Generic;

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
            new Morpheme("eleven") { Attributes = Attributes.A.Lexeme.Numeral.Cardinal },
            new Morpheme("emergency") { Attributes = Attributes.O.Lexeme.Noun },
            new Morpheme("end") { Attributes = Attributes.I.Lexeme.Verb.Monovalent },
            new Morpheme("good") { Attributes = Attributes.A.Lexeme.Adjective.Attributive },
            new Morpheme("have") { Attributes = Attributes.I.Lexeme.Verb.Bivalent },
            new Morpheme("i") { Attributes = Attributes.O.Lexeme.Pronoun.Subjective },
            new Morpheme("in") { Attributes = Attributes.E.Lexeme.Preposition },
            new Morpheme("news") { Attributes = Attributes.O.Lexeme.Noun },
            new Morpheme("not") { Attributes = Attributes.E.Lexeme.Adverb },
            new Morpheme("regarding") { Attributes = Attributes.E.Lexeme.Preposition },
            new Morpheme("some") { Attributes = Attributes.A.Lexeme.Determiner },
            new Morpheme("start") { Attributes = Attributes.I.Lexeme.Verb.Bivalent },
            new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle },
            new Morpheme("will") { Attributes = Attributes.I.Lexeme.Verb.Modal },
            new Morpheme("with") { Attributes = Attributes.E.Lexeme.Preposition },
            new Morpheme("world") { Attributes = Attributes.O.Lexeme.Noun },
            new Morpheme("year") { Attributes = Attributes.O.Lexeme.Noun },

            // Non-lexemes.
            new Morpheme(".") { Attributes = Attributes.U.NonLexeme.PunctuationMark.Period },
            new Morpheme("s") { Attributes = Attributes.O.NonLexeme.NounSuffix },
        };


    }
}
