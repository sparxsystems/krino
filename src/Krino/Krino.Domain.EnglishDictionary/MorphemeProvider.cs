using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.Domain.EnglishDictionary
{
    public class MorphemeProvider
    {
        public static EnglishAttributesModel AttributesModel { get; } = new EnglishAttributesModel();

        public static List<Morpheme> Morphemes => new List<Morpheme>()
        {
            // Lexemes.
            new Morpheme(AttributesModel, "a", EnglishAttributes.A.Lexeme.Determiner.IndefiniteArticle),
            new Morpheme(AttributesModel, "and", EnglishAttributes.U.Lexeme.Conjunction),
            new Morpheme(AttributesModel, "as", EnglishAttributes.U.Lexeme.Conjunction),
            
            new Morpheme(AttributesModel, "bad", EnglishAttributes.A.Lexeme.Adjective.Attributive),
            new Morpheme(AttributesModel, "because", EnglishAttributes.U.Lexeme.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme(AttributesModel, "been", EnglishAttributes.I.Lexeme.Verb.Form.PastParticiple),
            new Morpheme(AttributesModel, "book", EnglishAttributes.O.Lexeme.Noun),

            new Morpheme(AttributesModel, "climate", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(AttributesModel, "cycling", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(AttributesModel, "damage", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(AttributesModel, "damage", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(AttributesModel, "eleven", EnglishAttributes.A.Lexeme.Numeral.Cardinal),
            new Morpheme(AttributesModel, "emergency", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(AttributesModel, "end", EnglishAttributes.I.Lexeme.Verb.Form.Base | EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent),
            new Morpheme(AttributesModel, "good", EnglishAttributes.A.Lexeme.Adjective.Attributive),
            new Morpheme(AttributesModel, "grass", EnglishAttributes.O.Lexeme.Noun.Common.Concrete),
            new Morpheme(AttributesModel, "have", EnglishAttributes.I.Lexeme.Verb.Form.Base | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            
            new Morpheme(AttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun.Subjective),
            new Morpheme(AttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(AttributesModel, "is", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),

            new Morpheme(AttributesModel, "lately", EnglishAttributes.E.Lexeme.Adverb),
            new Morpheme(AttributesModel, "news", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(AttributesModel, "number", EnglishAttributes.O.Lexeme.Noun),
            new Morpheme(AttributesModel, "not", EnglishAttributes.E.Lexeme.Adverb),
            new Morpheme(AttributesModel, "of", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(AttributesModel, "on", EnglishAttributes.E.Lexeme.Preposition),
            
            new Morpheme(AttributesModel, "people", EnglishAttributes.O.Lexeme.Noun.Sememe.Number.Plural),
            new Morpheme(AttributesModel, "prohibit", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),

            new Morpheme(AttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(AttributesModel, "regarding", EnglishAttributes.E.Lexeme.Preposition),

            new Morpheme(AttributesModel, "say", EnglishAttributes.I.Lexeme.Verb.Form.Base | EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(AttributesModel, "small", EnglishAttributes.A.Lexeme.Adjective),
            new Morpheme(AttributesModel, "some", EnglishAttributes.A.Lexeme.Determiner),
            new Morpheme(AttributesModel, "start", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent),
            new Morpheme(AttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner.DefiniteArticle),

            new Morpheme(AttributesModel, "walk", EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent),
            new Morpheme(AttributesModel, "will", EnglishAttributes.I.Lexeme.Verb.Modal),
            new Morpheme(AttributesModel, "with", EnglishAttributes.E.Lexeme.Preposition),
            new Morpheme(AttributesModel, "world", EnglishAttributes.O.Lexeme.Noun),

            new Morpheme(AttributesModel, "year", EnglishAttributes.O.Lexeme.Noun.Common | EnglishAttributes.O.Lexeme.Noun.Countable),

            // Non-lexemes.
            new Morpheme(AttributesModel, ".", EnglishAttributes.U.NonLexeme.PunctuationMark.Period),
            new Morpheme(AttributesModel, ",", EnglishAttributes.U.NonLexeme.PunctuationMark.Comma),
            new Morpheme(AttributesModel, "ed", EnglishAttributes.I.NonLexeme.Suffix),
            new Morpheme(AttributesModel, "ing", EnglishAttributes.I.NonLexeme.Suffix),
            new Morpheme(AttributesModel, "s", EnglishAttributes.O.NonLexeme.Suffix),
        };
    }
}
