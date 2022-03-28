using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System.Collections.Generic;

namespace Krino.Domain.EnglishDictionary
{
    public class MorphemeProvider
    {
        public static List<IMorpheme> Morphemes => new List<IMorpheme>()
        {
            // Lexemes.
            new Morpheme("a", GrammarAttributes.Morpheme.Free.Functional.Determiner.IndefiniteArticle),
            new Morpheme("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            
            new Morpheme("bad", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun),

            new Morpheme("climate", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("cycling", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("eleven", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal),
            new Morpheme("emergency", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Common.Concrete),
            new Morpheme("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            
            new Morpheme("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective),
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),

            new Morpheme("lately", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("news", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("number", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation),
            new Morpheme("of", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("on", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            
            new Morpheme("people", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural),
            new Morpheme("prohibit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("prohibited", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("regarding", GrammarAttributes.Morpheme.Free.Functional.Preposition),

            new Morpheme("say", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("saying", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("small", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("some", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("start", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner.DefiniteArticle),

            new Morpheme("walk", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("walking", GrammarAttributes.Morpheme.Free.Lexical.Noun),

            new Morpheme("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("with", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("world", GrammarAttributes.Morpheme.Free.Lexical.Noun),

            new Morpheme("year", GrammarAttributes.Morpheme.Free.Lexical.Noun.Common | GrammarAttributes.Morpheme.Free.Lexical.Noun.Countable),

            // Non-lexemes.
            new Morpheme(".", GrammarAttributes.PunctuationMark.Period),
            new Morpheme(",", GrammarAttributes.PunctuationMark.Comma),
            new Morpheme("ed", GrammarAttributes.Morpheme.Bound.Suffix),
            new Morpheme("ing", GrammarAttributes.Morpheme.Bound.Suffix),
            new Morpheme("s", GrammarAttributes.Morpheme.Bound.Suffix),
        };
    }
}
