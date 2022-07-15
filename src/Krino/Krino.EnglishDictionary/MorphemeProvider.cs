using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.LinguisticStructures.Rules;
using Krino.ConstructiveGrammar.Morphology;
using Krino.EnglishGrammar.Morphology;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace Krino.EnglishDictionary
{
    public class MorphemeProvider
    {
        public static List<IMorpheme> Morphemes => new List<IMorpheme>()
        {
            // Determiniers
            new Morpheme("a", GrammarAttributes.Morpheme.Free.Functional.Determiner.IndefiniteArticle),
            new Morpheme("every", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("some", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner.DefiniteArticle),

            new Morpheme("my", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("your", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("his", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("her", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("its", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("their", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),


            // Conjunctions (onle 7 coordinating conjunctions)
            new Morpheme("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("but", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("for", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("nor", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("or", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("so", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("yet", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),

            new Morpheme("after", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("although", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("if", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("since", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("that", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("though", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("whenever", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("while", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),


            // Verbs
            new Morpheme("be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.FirstPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("are", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Plural.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Plural.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("was", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.FirstPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("were", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Plural.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Plural.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),

            new Morpheme("cycle", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("drive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("express", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson),
            new Morpheme("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple),

            new Morpheme("infringe", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("left", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past),
            new Morpheme("pay", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("prohibit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("say", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("should", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("start", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("walk", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("wear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),


            // Nouns
            new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("citizen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("climate", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("dutch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("eleven", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal),
            new Morpheme("emergency", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Common.Concrete),
            new Morpheme("individuality", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("man", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
            {
                Suppletions = new SuppletionBuilder()
                    .Add("men", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)
                    .Tolist(),
            },
            new Morpheme("men", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)
            {
                Suppletions = new SuppletionBuilder()
                    .Add("man", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
                    .Tolist(),
            },
            new Morpheme("news", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("number", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("people", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)
            {
                Suppletions = new SuppletionBuilder()
                    .Add("person", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
                    .Tolist(),
            },
            new Morpheme("person", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
            {
                Suppletions = new SuppletionBuilder()
                    .Add("people", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)
                    .Tolist(),
            },
            new Morpheme("right", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("road", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("royal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("rubber", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("school", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("student", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("suspect", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("tax", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("trace", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("uniform", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("world", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("year", GrammarAttributes.Morpheme.Free.Lexical.Noun.Common | GrammarAttributes.Morpheme.Free.Lexical.Noun.Countable),

            // Pronouns
            new Morpheme("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("you", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Second | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("he", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("she", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("it", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Neuter),
            new Morpheme("they", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective  | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            new Morpheme("me", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("him", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("her", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("us", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("them", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            new Morpheme("maine", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("yours", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Second | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("his", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("hers", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("theirs", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            
            // Adjectives
            new Morpheme("bad", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("long", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("prohibited", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("small", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            
            // Adverbs
            new Morpheme("actually", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("again", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("ahead", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("almost", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("already", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("also", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("always", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("below", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("certainly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("constantly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("currently", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("directly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("daily", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("early", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("easily", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("else", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("especially", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("eventually", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("ever", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("exactly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("extremely", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("finally", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fully", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("generally", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("here", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfPlace),
            new Morpheme("however", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("instead", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("lately", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("likely", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("mainly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("mostly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("nearly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("never", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("normally", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation),
            new Morpheme("often", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("probably", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("properly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("quickly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("rarely", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("rather", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("really", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("simply", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("sometimes", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("soon", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("specifically", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("therefore", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("thus", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("together", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("too", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("usually", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("very", GrammarAttributes.Morpheme.Free.Lexical.Adverb),



            // Colors
            new Morpheme("black", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("blue", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("brown", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("green", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("gray", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("grey", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("orange", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("pink", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("purple", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("red", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("yellow", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("white", GrammarAttributes.Morpheme.Free.Lexical.Noun),

            // Prepositions
            new Morpheme("about", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("above", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("across", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("after", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("again", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("along", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("among", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("around", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("it", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("before", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("behind", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("between", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("but", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("by", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("during", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("fro", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("from", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("of", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("off", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("on", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("over", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("past", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("regarding", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("than", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("through", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("to", GrammarAttributes.Morpheme.Free.Functional.Preposition | GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker),
            new Morpheme("until", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("up", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("upon", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("with", GrammarAttributes.Morpheme.Free.Functional.Preposition),



            // Non-lexemes.
            new Morpheme(".", GrammarAttributes.PunctuationMark.Period),
            new Morpheme(",", GrammarAttributes.PunctuationMark.Comma),

            // Noun plural
            new Morpheme("s", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular,
                    CanBindRule = !WordRules.WordEndsWithOneOf("s", "x", "z", "sh", "ch") &
                           EnglishWordRules.IsNounInBaseForm(),
                    TransformValue = Trans.Append("s"),
                }
            },
            new Morpheme("es", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular,
                    CanBindRule = WordRules.WordEndsWithOneOf("s", "x", "z", "sh", "ch") &
                           EnglishWordRules.IsNounInBaseForm(),
                    TransformValue = Trans.Append("es"),
                }
            },

            // Noun possessive
            new Morpheme("'s", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun.Possessive,
                    AttributesToDrop = 0,
                    CanBindRule = WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Noun),
                    TransformValue = Trans.Append("'s"),
                }
            },


            // Verb present tense 3rd person singular
            new Morpheme("s", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular,
                    CanBindRule = !WordRules.WordEndsWithOneOf("s", "x", "z", "sh", "ch") &
                           EnglishWordRules.IsVerbInBaseForm(),
                    TransformValue = Trans.Append("s"),
                }
            },
            new Morpheme("es", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular,
                    CanBindRule = WordRules.WordEndsWithOneOf("s", "x", "z", "sh", "ch") &
                           EnglishWordRules.IsVerbInBaseForm(),
                    TransformValue = Trans.Append("es"),
                }
            },

            // Verb past-tense form
            new Morpheme("ed", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base,
                    CanBindRule = EnglishWordRules.IsVerbInBaseForm() & !EnglishWordRules.IsIrregularVerb(),
                    TransformValue = Trans.Append("ed"),
                }
            },

            // Verb past-participle form
            new Morpheme("ed", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base,
                    CanBindRule = EnglishWordRules.IsVerbInBaseForm() & !EnglishWordRules.IsIrregularVerb(),
                    TransformValue = Trans.Append("ed"),
                }
            },

            // Verb ing form
            new Morpheme("ing", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base,
                    CanBindRule = EnglishWordRules.IsVerbInBaseForm(),
                    TransformValue = Trans
                         // If it ends with short vowel and consonant then double the last consonant. e.g. putting.
                        .If(EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel, Phoneme.Consonant),
                                Trans.Then(EnglishWordTrans.DoubleLastLetter(), Trans.Append("ing")))
                        // If it ends with 'e' then drop the 'e' e.g. joking.
                        .Else(Trans
                            .If(RuleMaker.EndsWithStr("e"),
                                Trans.Then(EnglishWordTrans.DropLastLetter(), Trans.Append("ing")))
                            .Else(Trans.Append("ing"))),
                }
            },
        };
    }
}
