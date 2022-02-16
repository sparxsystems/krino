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
            new Morpheme("a", GrammarAttributes.Morpheme.A.Free.Determiner.IndefiniteArticle),
            new Morpheme("and", GrammarAttributes.Morpheme.U.Free.Conjunction),
            new Morpheme("as", GrammarAttributes.Morpheme.U.Free.Conjunction),
            
            new Morpheme("bad", GrammarAttributes.Morpheme.A.Free.Adjective),
            new Morpheme("because", GrammarAttributes.Morpheme.U.Free.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("been", GrammarAttributes.Morpheme.I.Free.Verb.Form.PastParticiple),
            new Morpheme("book", GrammarAttributes.Morpheme.O.Free.Noun),

            new Morpheme("climate", GrammarAttributes.Morpheme.O.Free.Noun),
            new Morpheme("cycling", GrammarAttributes.Morpheme.O.Free.Noun),
            new Morpheme("damage", GrammarAttributes.Morpheme.O.Free.Noun),
            new Morpheme("damage", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            new Morpheme("eleven", GrammarAttributes.Morpheme.A.Free.Numeral.Cardinal),
            new Morpheme("emergency", GrammarAttributes.Morpheme.O.Free.Noun),
            new Morpheme("end", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            new Morpheme("good", GrammarAttributes.Morpheme.A.Free.Adjective),
            new Morpheme("grass", GrammarAttributes.Morpheme.O.Free.Noun.Common.Concrete),
            new Morpheme("have", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            
            new Morpheme("i", GrammarAttributes.Morpheme.O.Free.Pronoun.Subjective),
            new Morpheme("in", GrammarAttributes.Morpheme.E.Free.Preposition),
            new Morpheme("is", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.ThirdPersonSingular),

            new Morpheme("lately", GrammarAttributes.Morpheme.E.Free.Adverb),
            new Morpheme("news", GrammarAttributes.Morpheme.O.Free.Noun),
            new Morpheme("number", GrammarAttributes.Morpheme.O.Free.Noun),
            new Morpheme("not", GrammarAttributes.Morpheme.E.Free.Adverb),
            new Morpheme("of", GrammarAttributes.Morpheme.E.Free.Preposition),
            new Morpheme("on", GrammarAttributes.Morpheme.E.Free.Preposition),
            
            new Morpheme("people", GrammarAttributes.Morpheme.O.Free.Noun.Sememe.Number.Plural),
            new Morpheme("prohibit", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),

            new Morpheme("read", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            new Morpheme("regarding", GrammarAttributes.Morpheme.E.Free.Preposition),

            new Morpheme("say", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            new Morpheme("small", GrammarAttributes.Morpheme.A.Free.Adjective),
            new Morpheme("some", GrammarAttributes.Morpheme.A.Free.Determiner),
            new Morpheme("start", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            new Morpheme("the", GrammarAttributes.Morpheme.A.Free.Determiner.DefiniteArticle),

            new Morpheme("walk", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.I.Free.Verb.Form.Base),
            new Morpheme("will", GrammarAttributes.Morpheme.I.Free.Verb.Modal),
            new Morpheme("with", GrammarAttributes.Morpheme.E.Free.Preposition),
            new Morpheme("world", GrammarAttributes.Morpheme.O.Free.Noun),

            new Morpheme("year", GrammarAttributes.Morpheme.O.Free.Noun.Common | GrammarAttributes.Morpheme.O.Free.Noun.Countable),

            // Non-lexemes.
            new Morpheme(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period),
            new Morpheme(",", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Comma),
            new Morpheme("ed", GrammarAttributes.Morpheme.I.Bound.Suffix),
            new Morpheme("ing", GrammarAttributes.Morpheme.I.Bound.Suffix),
            new Morpheme("s", GrammarAttributes.Morpheme.O.Bound.Suffix),
        };
    }
}
