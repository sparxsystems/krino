using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Attributing;
using System.Collections.Generic;

namespace Krino.GretaTest
{
    public static class MorphemeProvider
    {
        public static List<Morpheme> Morphemes => new List<Morpheme>()
        {
            // Lexemes.
            new Morpheme("a", Attributes.A.Lexeme.Determiner.IndefiniteArticle),
            new Morpheme("and", Attributes.U.Lexeme.Conjunction),
            new Morpheme("as", Attributes.U.Lexeme.Conjunction),
            new Morpheme("climate", Attributes.O.Lexeme.Noun),
            new Morpheme("bad", Attributes.A.Lexeme.Adjective.Attributive),
            new Morpheme("been", Attributes.I.Lexeme.Verb.Sememe.Tense.Past | Attributes.I.Lexeme.Verb.Form.PastParticiple),
            new Morpheme("eleven", Attributes.A.Lexeme.Numeral.Cardinal),
            new Morpheme("emergency", Attributes.O.Lexeme.Noun),
            new Morpheme("end", Attributes.I.Lexeme.Verb.Form.Infinitive | Attributes.I.Lexeme.Verb.Valency.Monovalent),
            new Morpheme("good", Attributes.A.Lexeme.Adjective.Attributive),
            new Morpheme("have", Attributes.I.Lexeme.Verb.Form.Infinitive | Attributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme("i", Attributes.O.Lexeme.Pronoun.Subjective),
            new Morpheme("in", Attributes.E.Lexeme.Preposition),
            new Morpheme("lately", Attributes.E.Lexeme.Adverb),
            new Morpheme("news", Attributes.O.Lexeme.Noun),
            new Morpheme("number", Attributes.O.Lexeme.Noun),
            new Morpheme("not", Attributes.E.Lexeme.Adverb),
            new Morpheme("of", Attributes.E.Lexeme.Preposition),
            new Morpheme("people", Attributes.O.Lexeme.Noun.Sememe.Number.Plural),
            new Morpheme("regarding", Attributes.E.Lexeme.Preposition),
            new Morpheme("say", Attributes.I.Lexeme.Verb.Form.Infinitive | Attributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme("small", Attributes.A.Lexeme.Adjective),
            new Morpheme("some", Attributes.A.Lexeme.Determiner),
            new Morpheme("start", Attributes.I.Lexeme.Verb.Form.Infinitive | Attributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme("the", Attributes.A.Lexeme.Determiner.DefiniteArticle),
            new Morpheme("will", Attributes.I.Lexeme.Verb.Modal),
            new Morpheme("with", Attributes.E.Lexeme.Preposition),
            new Morpheme("world", Attributes.O.Lexeme.Noun),
            new Morpheme("year", Attributes.O.Lexeme.Noun),

            // Non-lexemes.
            new Morpheme(".", Attributes.U.NonLexeme.PunctuationMark.Period),
            new Morpheme(",", Attributes.U.NonLexeme.PunctuationMark.Comma),
            new Morpheme("ing", Attributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous),
            new Morpheme("s", Attributes.O.NonLexeme.Suffix),
        };


    }
}
