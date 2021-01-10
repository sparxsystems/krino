using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.GretaTest
{
    public static class MorphemeProvider
    {
        private static EnglishAttributesModel myEnglishAttributesModel = new EnglishAttributesModel();

        public static List<Morpheme> Morphemes => new List<Morpheme>()
        {
            // Lexemes.
            new Morpheme(myEnglishAttributesModel, "a", EnglishAttributes.A.Lexeme.Determiner.IndefiniteArticle),
            new Morpheme(myEnglishAttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction),
            new Morpheme(myEnglishAttributesModel, "as", EnglishAttributes.U.Lexeme.Conjunction),
            new Morpheme(myEnglishAttributesModel, "climate", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(myEnglishAttributesModel, "bad", EnglishAttributes.A.Lexeme.Adjective.Attributive),
            new Morpheme(myEnglishAttributesModel, "been", EnglishAttributes.I.Lexeme.Verb.Sememe.Tense.Past | EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple),
            new Morpheme(myEnglishAttributesModel, "eleven", EnglishAttributes.A.Lexeme.Numeral.Cardinal),
            new Morpheme(myEnglishAttributesModel, "emergency", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(myEnglishAttributesModel, "end", EnglishAttributes.I.Lexeme.Verb.Form.Infinitive | EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent),
            new Morpheme(myEnglishAttributesModel, "good", EnglishAttributes.A.Lexeme.Adjective.Attributive),
            new Morpheme(myEnglishAttributesModel, "have", EnglishAttributes.I.Lexeme.Verb.Form.Infinitive | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(myEnglishAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun.Subjective),
            new Morpheme(myEnglishAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(myEnglishAttributesModel, "lately", EnglishAttributes.E.Lexeme.Adverb),
            new Morpheme(myEnglishAttributesModel, "news", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(myEnglishAttributesModel, "number", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(myEnglishAttributesModel, "not", EnglishAttributes.E.Lexeme.Adverb),
            new Morpheme(myEnglishAttributesModel, "of", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(myEnglishAttributesModel, "people", EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural),
            new Morpheme(myEnglishAttributesModel, "regarding", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(myEnglishAttributesModel, "say", EnglishAttributes.I.Lexeme.Verb.Form.Infinitive | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(myEnglishAttributesModel, "small", EnglishAttributes.A.Lexeme.Adjective),
            new Morpheme(myEnglishAttributesModel, "some", EnglishAttributes.A.Lexeme.Determiner),
            new Morpheme(myEnglishAttributesModel, "start", EnglishAttributes.I.Lexeme.Verb.Form.Infinitive | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(myEnglishAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),
            new Morpheme(myEnglishAttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal),
            new Morpheme(myEnglishAttributesModel, "with", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(myEnglishAttributesModel, "world", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(myEnglishAttributesModel, "year", EnglishAttributes.O.Lexeme.Noun.Common | EnglishAttributes.O.Lexeme.Noun.Countable),

            // Non-lexemes.
            new Morpheme(myEnglishAttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark.Period),
            new Morpheme(myEnglishAttributesModel, ",", EnglishAttributes.U.NonLexeme.PunctuationMark.Comma),
            new Morpheme(myEnglishAttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix.Sememe.Aspect.Continuous),
            new Morpheme(myEnglishAttributesModel, "s", EnglishAttributes.O.NonLexeme.Suffix),
        };


    }
}
