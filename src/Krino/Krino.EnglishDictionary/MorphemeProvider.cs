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

            new Morpheme("a", GrammarAttributes.Morpheme.Free.Functional.Determiner.IndefiniteArticle),
            new Morpheme("able", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("about", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("above", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("across", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("act", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("add", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("afraid", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("after", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("after", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("again", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("again", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("age", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("ago", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("agree", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("ahead", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("air", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("all", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("allow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("almost", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("alone", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("alone", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("along", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("already", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("also", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("although", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("always", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("among", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("amount", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("an", GrammarAttributes.Morpheme.Free.Functional.Determiner.IndefiniteArticle),
            new Morpheme("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("anger", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("anger", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("angry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("another", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("another", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("answer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("any", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("anyone", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("anything", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("anytime", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("appear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("apple", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("area", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("arm", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("army", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("arrive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("around", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("art", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("ask", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("at", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("attack", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("aunt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("autumn", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("away", GrammarAttributes.Morpheme.Free.Lexical.Adverb),

            new Morpheme("baby", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("base", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("back", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bad", GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddSuppletion(new Morpheme("worse", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative))
                .AddSuppletion(new Morpheme("worst", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative)),
            new Morpheme("badly", GrammarAttributes.Morpheme.Free.Lexical.Adverb)
                .AddSuppletion(new Morpheme("worse", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Comparative))
                .AddSuppletion(new Morpheme("worst", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Superlative)),
            new Morpheme("bag", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("ball", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bank", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("basket", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bath", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary)
                .AddSuppletion(new Morpheme("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.FirstPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary))
                .AddSuppletion(new Morpheme("are", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Plural.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Plural.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary))
                .AddSuppletion(new Morpheme("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary))
                .AddSuppletion(new Morpheme("was", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.FirstPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary))
                .AddSuppletion(new Morpheme("were", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Plural.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Plural.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary))
                .AddSuppletion(new Morpheme("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary)),
            new Morpheme("bean", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("beauty", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("beer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bed", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("behave", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("before", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("begin", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("behind", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("bell", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("below", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("besides", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("between", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("big", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("bird", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("birth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bite", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("black", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("bleed", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("block", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("block", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("blood", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("blow", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("blow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("blue", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("board", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("board", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("boat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("body", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("boil", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("bone", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("border", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("born", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("borrow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("both", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("both", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective),
            //??? aky druh conjunction
            new Morpheme("both", GrammarAttributes.Morpheme.Free.Functional.Conjunction),
            new Morpheme("bottle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bottom", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bowl", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("box", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("box", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("boy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("branch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("brave", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("bread", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("break", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("break", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("breakfast", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("breathe", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("bridge", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bright", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("bring", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("brother", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("brown", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("brush", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("build", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("burn", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("business", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bus", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("busy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("but", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("but", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("buy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("buy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("by", GrammarAttributes.Morpheme.Free.Functional.Preposition),
                       
            new Morpheme("cake", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("call", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("call", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("can", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("candle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cap", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("car", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("card", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("care", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("care", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("carry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("case", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("catch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("catch", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("central", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("century", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("certain", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("chair", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("change", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("chase", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cheap", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cheese", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("chicken", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("child", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("chocolate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("choice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("choose", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("circle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("city", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("citizen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("class", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("clever", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("clean", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("clear", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("clear", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("clear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("climate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("climb", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("clock", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cloth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cloud", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("close", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("coffee", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("coat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("coat", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("coin", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cold", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("collect", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("collect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("colour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("color", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("comb", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("comb", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("come", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("comfort", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("common", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("compare", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("compare", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("complete", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("complete", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("computer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("condition", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("constant", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("constant", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("continue", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("control", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("control", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cook", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cook", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("cool", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cooper", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("corn", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("corner", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("correct", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("correct", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cost", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cost", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("contain", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("count", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("country", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("course", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cover", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cover", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("crash", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("crash", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("crease", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("crease", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cross", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cross", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("cup", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("current", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cut", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cut", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cycle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("cycle", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),

            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("dance", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),            
            new Morpheme("dance", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("danger", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dark", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("dark", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("daugther", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("day", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dead", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dead", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("decide", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("decrease", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("decrease", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("deep", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("deep", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("deep", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("deer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("depend", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("desk", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("destroy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("develop", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("die", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("die", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("different", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("dinner", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("direct", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("dish", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("do", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("dog", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("door", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("draw", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("dream", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dream", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("dress", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dress", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("drink", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("drink", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("drive", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("drive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("drop", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("drop", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("dry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("dry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("duck", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("during", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("dust", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dust", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("duty", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("destroy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("dedicate", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("dutch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dutch", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            new Morpheme("each", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("each", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("each", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("ear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("early", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("early", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("earn", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("earth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("east", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("east", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("east", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("easy", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("easy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("eat", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("education", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("effect", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("effect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("egg", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("eight", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal),
            new Morpheme("either", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("either", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("either", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("electric", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("electric", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("elephant", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("eleven", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal),
            new Morpheme("else", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("emergency", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("empty", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("empty", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("empty", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("enemy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("enjoy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("enough", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("enough", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("enough", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("enter", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("equal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("equal", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("equal", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("entrance", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("entrance", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("escape", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("escape", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("especial", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("even", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("even", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("even", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("evening", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("event", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("ever", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("every", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("exact", GrammarAttributes.Morpheme.Free.Lexical.Adjective),            
            new Morpheme("exact", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("exactly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("examination", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("example", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //??? ci je to Subordinating
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            //???
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("excite", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("exercise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("exercise", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("expect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("expense", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("explain", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("express", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("express", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("express", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("extreme", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("extreme", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("eye", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),

            new Morpheme("face", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("face", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("fact", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fail", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fail", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fall", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fall", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("false", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("false", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("family", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("famous", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("far", GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddSuppletion(new Morpheme("further", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative))
                .AddSuppletion(new Morpheme("farther", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative))
                .AddSuppletion(new Morpheme("furthest", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative))
                .AddSuppletion(new Morpheme("farthest", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative)),
            new Morpheme("far", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("farm", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("farm", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("father", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fat", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("feed", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("feed", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("feel", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("feel", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("female", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fever", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("few", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("flight", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fill", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("film", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("film", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("final", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("final", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("find", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("find", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("finger", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("finger", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("finish", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("finish", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("fire", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fire", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? toto je aj determiner a adverb
            new Morpheme("first", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("flag", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("flag", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("flat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("flat", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("flat", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("float", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("float", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("floor", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("floor", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("flour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("flour", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("flower", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("flower", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fly", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fly", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fly", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fold", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fold", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("food", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fool", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fool", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fool", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("foot", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("foot", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? je to aj preposition
            new Morpheme("for", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("force", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("force", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("foreign", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("forest", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("forget", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("forgive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("fork", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fork", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("form", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("form", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("fox", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("frame", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("frame", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("free", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("free", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("free", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("freeze", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("freeze", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fresh", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fresh", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("friend", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("friendly", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("from", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("front", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("front", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("front", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("fruit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fruit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //??? ako to napisat full - fully
            new Morpheme("full", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("full", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fully", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fun", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fun", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("furniture", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("further", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //???
            new Morpheme("further", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("future", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("future", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            
            new Morpheme("game", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("game", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("game", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("garden", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("garden", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("gate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("general", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("general", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? meni sa forma na men
            new Morpheme("gentleman", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("get", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("gift", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("gift", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("give", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("glad", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("glass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("glass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("go", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("goat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("god", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("gold", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("gold", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddSuppletion(new Morpheme("better", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative))
                .AddSuppletion(new Morpheme("best", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative)),
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("grave", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("grave", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("great", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("great", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("great", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("green", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("grey", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("gray", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("ground", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("ground", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("ground", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),            
            //???
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("group", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("group", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("grow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("gun", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("gun", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("hair", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("half", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("half", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("half", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("hall", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hammer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hammer", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("hand", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hand", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("happen", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("happy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hard", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("hard", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hate", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent)
                .AddSuppletion(new Morpheme("has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent))
                .AddSuppletion(new Morpheme("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent)),
            new Morpheme("he", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("head", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("head", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("health", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("heavy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("heavy", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? toto je aj exclamation
            new Morpheme("hello", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? toto je aj exclamation
            new Morpheme("help", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("help", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("heart", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? ci heart je naozaj aj verb
            new Morpheme("heart", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("heaven", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("height", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("her", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("her", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("here", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfPlace),
            new Morpheme("hers", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("hide", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hide", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("high", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("high", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("him", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("his", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("his", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("hit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("hobby", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hold", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hold", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("hole", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hole", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("holiday", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("home", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("home", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("home", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hope", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hope", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("horse", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("horse", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("hospital", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hot", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("hot", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("hotel", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("house", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("house", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("how", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("however", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("hundred", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal),
            new Morpheme("hungry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hurry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hurry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("husband", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hurt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hurt", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hurt", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),

            new Morpheme("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("ice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("ice", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("idea", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? je to aj noun
            new Morpheme("if", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("if", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("image", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("imagine", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("important", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? je to aj noun
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            // napr. ked je nieco 'in'
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("individual", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? toto moze byt trivalent
            new Morpheme("infringe", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("instead", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("into", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("introduce", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            //???
            new Morpheme("invent", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("iron", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("iron", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("iron", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            //??? pozvat niekoho niekam
            new Morpheme("invite", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("island", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("it", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Neuter),
            new Morpheme("its", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),

            new Morpheme("jelly", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("job", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("join", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? pridat cukor do kolaca
            new Morpheme("join", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("juice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("juice", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("jump", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("jump", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("just", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            
            new Morpheme("keep", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("keep", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("key", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("key", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("key", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("kid", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("kid", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("kid", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("kill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? zabit niekoho niecim
            new Morpheme("kill", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("kind", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("kind", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("king", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("kitchen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("knee", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("knee", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("knife", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("knife", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("knock", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? klopat na dvere
            new Morpheme("knock", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("know", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("know", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),

            new Morpheme("ladder", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? ladies
            new Morpheme("lady", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("lamp", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("land", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("land", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("large", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("last", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("last", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("last", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //??? neviem ci moze byt bivalent
            new Morpheme("last", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("late", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("late", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? pripona ly
            new Morpheme("lately", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("laugh", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? smejem sa na kom na com
            new Morpheme("laugh", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("lazy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("lead", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? 
            new Morpheme("lead", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("leaf", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? kedy je iba bivalent
            new Morpheme("learn", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            //??? necham niekoho niekde
            new Morpheme("leave", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent)
                .AddSuppletion(new Morpheme("left", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent))
                .AddSuppletion(new Morpheme("left", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent)),
            new Morpheme("leg", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("leg", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("left", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("left", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("left", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("lend", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("length", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("less", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("less", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("lesson", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? je let aj monovalent aj bivalent
            new Morpheme("let", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("letter", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("letter", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("library", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? lie ma dva vyznamy
            new Morpheme("lie", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? 
            new Morpheme("lie", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("life", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
                .AddSuppletion(new Morpheme("lives", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)),
            new Morpheme("light", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("light", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("light", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? 
            new Morpheme("light", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("like", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("lion", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("lip", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("list", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("list", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("listen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("listen", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("little", GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddSuppletion(new Morpheme("less", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative))
                .AddSuppletion(new Morpheme("least", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative)),
            new Morpheme("little", GrammarAttributes.Morpheme.Free.Lexical.Adverb)
                .AddSuppletion(new Morpheme("less", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Comparative))
                .AddSuppletion(new Morpheme("least", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Superlative)),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("little", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //??? live  ma dva vyznamy
            new Morpheme("live", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("live", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("lock", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("lock", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("lonely", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("long", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("long", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfTime),
            new Morpheme("long", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("look", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("lose", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("lot", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("lot", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("lot", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("love", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("love", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("low", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("low", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("low", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? 
            new Morpheme("low", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //??? pripona er a ma dva vyznamy
            new Morpheme("lower", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? pripona er a ma dva vyznamy
            new Morpheme("lower", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("luck", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),

            new Morpheme("machine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("machine", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("main", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("main", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("maine", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("make", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("make", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent)
                .AddSuppletion(new Morpheme("made", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent))
                .AddSuppletion(new Morpheme("made", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent)),
            new Morpheme("male", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("male", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("man", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
                .AddSuppletion(new Morpheme("men", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("many", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("map", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("map", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("mark", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mark", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("marked", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("marked", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("marry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("matter", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("matter", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("may", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("me", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("meal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("mean", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("mean", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("mean", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("measure", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("measure", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("meat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("medicine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("meet", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("meet", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("member", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("mention", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mention", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("method", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("middle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("middle", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("milk", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("milk", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("mill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mill", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? million number
            new Morpheme("mind", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mind", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("mine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? aka forma pronoun
            new Morpheme("mine", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive),
            //???
            new Morpheme("mind", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("minute", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("minute", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("miss", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("miss", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("mistake", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mistake", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("mix", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mix", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("model", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("model", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("modern", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("moment", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("money", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("monkey", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("month", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("moon", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("moon", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("more", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("more", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("morning", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("most", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("most", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("mother", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mother", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("mountain", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("mouse", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("mouth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("mouth", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("move", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("move", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("much", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("much", GrammarAttributes.Morpheme.Free.Lexical.Adverb)
                .AddSuppletion(new Morpheme("more", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Comparative))
                .AddSuppletion(new Morpheme("most", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Superlative)),
            new Morpheme("much", GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddSuppletion(new Morpheme("more", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative))
                .AddSuppletion(new Morpheme("most", GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative)),
            new Morpheme("music", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("must", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("must", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("my", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),

            new Morpheme("name", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("name", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("narrow", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("narrow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("nation", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("nature", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("near", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("near", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            // predlozka 'pri'
            new Morpheme("near", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("neck", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("need", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("need", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("needle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("needle", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("neighbour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("neither", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("neither", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("net", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("net", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("net", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("net", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("never", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            // ??? singular?
            new Morpheme("new", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("news", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("next", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("next", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("next", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("nice", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("night", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? nine is number
            new Morpheme("no", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("no", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation),
            new Morpheme("no", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("noble", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("noble", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("noise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? none je aj pronoun aky
            new Morpheme("none", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation),
            new Morpheme("nor", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("normal", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("normal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("north", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("north", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("north", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("nose", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation),
            new Morpheme("notice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("notice", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? asi je to adverb casu
            new Morpheme("now", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("number", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),

            //???
            new Morpheme("obey", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("object", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("object", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("ocean", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("of", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("off", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("offer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("offer", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("office", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("often", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("oil", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("oil", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("old", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("on", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //??? one je aj number a pronoun 
            new Morpheme("one", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //??? only je aj conjunction
            new Morpheme("only", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("only", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("open", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("open", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("open", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("opposite", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("opposite", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("opposite", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("or", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("orange", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            //??? other je aj pronoun aky
            new Morpheme("other", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("other", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("our", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("out", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("out", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("out", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("out", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("over", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //??? own je aj pronoun aky
            new Morpheme("own", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("own", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("page", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("page", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pain", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pain", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("paint", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("paint", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pair", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pair", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pan", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pan", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("paper", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("paper", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("parent", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("parent", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("park", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("park", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("part", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("part", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("partner", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("partner", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("party", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("party", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? past je aj noun
            new Morpheme("past", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("past", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("past", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("path", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("pay", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("peace", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("pen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pen", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pencil", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pencil", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pepper", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pepper", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("per", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("perfect", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("perfect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("perfect", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("period", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("period", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("period", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("person", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
                .AddSuppletion(new Morpheme("people", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)),
            new Morpheme("petrol", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("photograph", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("photograph", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("piano", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("piano", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("piano", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("pick", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pick", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("picture", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("picture", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("piece", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("pig", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pig", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pill", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pin", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pin", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pink", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            //???
            new Morpheme("pink", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pink", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("place", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("place", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("plane", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("plane", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("plane", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("plant", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("plant", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("plastic", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("plastic", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("plate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("plate", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("play", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("play", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? please je aj exclamation
            new Morpheme("please", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? plenty je aj pronoun aky
            new Morpheme("plenty", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? plenty je singular aj plural
            new Morpheme("plenty", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("pocket", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pocket", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("point", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("point", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("poison", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("poison", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("police", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("police", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("polite", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("pool", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pool", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("poor", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("popular", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("position", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("position", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("possible", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("possible", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("potato", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pour", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("power", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("power", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("present", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("present", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("present", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("press", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("press", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("pretty", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("pretty", GrammarAttributes.Morpheme.Free.Lexical.Adjective),            
            //???
            new Morpheme("prevent", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("price", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("price", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("prince", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("prison", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("private", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("private", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("prize", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("prize", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("prize", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("probably", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("probable", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("probable", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("problem", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("problem", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("produce", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("produce", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("promise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("promise", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("proper", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("protect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("provide", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("prohibit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("public", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("public", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("purple", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("pull", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("pull", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("punish", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("punish", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("pupil", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("push", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("push", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("put", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("queen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("question", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("question", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("quick", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("quick", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("quick", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("quiet", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("quiet", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("quiet", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            new Morpheme("radio", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("rain", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("rain", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("raise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("raise", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("rare", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("rather", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("reach", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("reach", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("react", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("real", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("real", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("real", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("receive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("record", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("record", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("red", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            //???
            new Morpheme("regarding", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("remember", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("remind", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("remove", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("rent", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("rent", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("repair", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("repair", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("repeat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("repeat", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("replay", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("replay", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("report", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("report", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("response", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("rest", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("rest", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("restaurant", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("result", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("result", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("return", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("return", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("rice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("rich", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("ride", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("ride", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("right", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("right", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("right", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
             //???
            new Morpheme("right", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("ring", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("ring", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("rise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("rise", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("road", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("rob", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("rock", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("rock", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("room", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("room", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("round", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("round", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("round", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("round", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("round", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("royal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("rubber", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("rubber", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("rude", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("rule", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("rule", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("ruler", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("run", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("run", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("rush", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             //???
            new Morpheme("rush", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("sad", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("safe", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("safe", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("sail", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sail", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("salt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("salt", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("salt", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? same je aj pronoun
            new Morpheme("same", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("same", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("sand", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sand", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? save je aj conjunction ale aky
            new Morpheme("save", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("save", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("save", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("say", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("say", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("school", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("school", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("science", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? scissors je pomnozne ako to oznacit
            new Morpheme("scissors", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("search", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("search", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("seat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("seat", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("second", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("second", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("second", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("see", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("see", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? seem linking verb
            new Morpheme("sell", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sell", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("send", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sentence", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sentence", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("serve", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("serve", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? seven je number
            //??? several je determiner a aj pronoun
            new Morpheme("several", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("sex", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sex", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shade", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shade", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shadow", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("shake", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shake", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shape", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shape", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("share", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("share", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sharp", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sharp", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("sharp", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("she", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("sheep", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sheet", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("shelf", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("shine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shine", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("ship", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("ship", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shirt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("shoe", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shoe", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shoot", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shoot", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shop", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shop", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("short", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("short", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("short", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("short", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("should", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("shoulder", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shoulder", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("shout", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("shout", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("show", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sick", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sick", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sick", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("side", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("side", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("signal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("signal", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("silence", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("silence", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("silly", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("silly", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("silver", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("silver", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("silver", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("similar", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("simple", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("single", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("single", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("single", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("since", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("since", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("since", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("sing", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sing", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sink", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sink", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sink", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("sister", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //??? six je number
            new Morpheme("size", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("size", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("skill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("skin", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("skin", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("skirt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("skirt", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sky", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? vyraz pri sporte
            new Morpheme("sky", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sleep", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sleep", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("slip", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("slip", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("slow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("slow", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("slow", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("small", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("small", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("small", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("smell", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("smell", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("smile", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("smile", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("smoke", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("smoke", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("snow", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("snow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("so", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("soap", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("soap", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sock", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sock", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("soft", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? some je aj pronoun ale aky
            new Morpheme("some", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("sometimes", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("son", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),            
            new Morpheme("soon", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? sorry je aj exclamation
            new Morpheme("sorry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("sound", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),                        
            //???
            new Morpheme("sound", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("sound", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("sound", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("soup", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),                        
            //???
            new Morpheme("soup", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("south", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("south", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("south", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("space", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("space", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("speak", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("special", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("special", GrammarAttributes.Morpheme.Free.Lexical.Adjective),            
            new Morpheme("specific", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("speed", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("speed", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("spell", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("spell", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("spend", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("spend", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("spoon", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("spoon", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sport", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sport", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("spread", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("spread", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("spring", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("spring", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("square", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("square", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("square", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("square", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("stamp", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("stamp", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("stand", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("stand", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("star", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("star", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("start", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("start", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("station", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("station", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("stay", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("stay", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("steal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("steal", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("steam", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("steam", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("step", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("step", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("still", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("still", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("still", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("still", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("stomach", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("stomach", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("stone", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("stone", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("stop", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("stop", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("store", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("store", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("storm", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("storm", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("story", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("strange", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("street", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("street", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("strong", GrammarAttributes.Morpheme.Free.Lexical.Adjective),                       
            new Morpheme("structure", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("structure", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("student", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("study", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("study", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("stupid", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("stupid", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("subject", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("subject", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("subject", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("substance", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("success", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sudden", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? such je pronoun a determiner
            new Morpheme("sugar", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sugar", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("summer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sun", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("sun", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("suport", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("suport", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sure", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("sure", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("surprise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("surprise", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("suspect", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sweet", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sweet", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("swim", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("swim", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("sword", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),

            new Morpheme("table", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("table", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("take", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("take", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("talk", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("talk", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tall", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("taste", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("taste", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tax", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("taxi", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("taxi", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tea", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("teach", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("team", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("team", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("tear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("telephone", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("telephone", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("television", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("tell", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? ten je number
            new Morpheme("tennis", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("terrible", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("test", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("test", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("than", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("that", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner.DefiniteArticle),
            new Morpheme("their", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("theirs", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("then", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("then", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("them", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),            
            new Morpheme("therefore", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? these je pronoun a determiner
            new Morpheme("they", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective  | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("thick", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("thick", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("thick", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("thin", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("thin", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("thin", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("thing", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("think", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("think", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? third je ordinary number
            //??? this je pronoun a determiner
            new Morpheme("this", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? those je pronoun a determiner
            new Morpheme("though", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("though", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("threat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? three je number
            new Morpheme("through", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("through", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("through", GrammarAttributes.Morpheme.Free.Lexical.Adjective),            
            new Morpheme("thus", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("tidy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("tidy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tidy", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("tie", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("tie", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("title", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("title", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("to", GrammarAttributes.Morpheme.Free.Functional.Preposition | GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker),
            new Morpheme("to", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("today", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("today", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("toe", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("toe", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),           
            new Morpheme("together", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("together", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("tomorrow", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("tomorrow", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("tonight", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("tonight", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("too", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("tool", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("tooth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("top", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("top", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("top", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("total", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("total", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("total", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("touch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("touch", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("town", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("trace", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("train", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("train", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tram", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("travel", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("travel", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("tree", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("trouble", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("trouble", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("true", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("true", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("true", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("trust", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("trust", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("twice", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("try", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("try", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("turn", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("turn", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("type", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("type", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("uncle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("under", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("under", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("under", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("understand", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("unit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("until", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("up", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("upon", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("us", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("use", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("use", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("usual", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            new Morpheme("vegetable", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("very", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("village", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("voice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("voice", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("visit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("visit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("wait", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wait", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wake", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wake", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("walk", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("walk", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("wall", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("want", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("want", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("warm", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("warm", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("warm", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wash", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wash", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("waste", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("waste", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("watch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("watch", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("water", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("water", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("way", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("way", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? we je pronoun aky
            new Morpheme("weak", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("wear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("wear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("weather", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("weather", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wedding", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("week", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("weight", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("weight", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? welcome je aj exclamation
            new Morpheme("welcome", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("welcome", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("welcome", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? well je aj exclamation
            new Morpheme("well", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("well", GrammarAttributes.Morpheme.Free.Lexical.Adverb)
                .AddSuppletion(new Morpheme("better", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Comparative))
                .AddSuppletion(new Morpheme("best", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Superlative)),
            new Morpheme("well", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("west", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("west", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("west", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("wet", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wet", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wet", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? what je pronoun a aj determiner
            new Morpheme("wheel", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wheel", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //??? when je pronoun a aj conjunction
            new Morpheme("when", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("whenever", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            //??? where je a aj conjunction
            new Morpheme("where", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? which je pronoun a aj determiner
            //??? while je aj conjunction
            new Morpheme("while", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),            
            new Morpheme("while", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("white", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("white", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? who je pronoun
            //??? why je aj exclamation
            new Morpheme("why", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("why", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("wide", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("wide", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("wide", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("wife", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("wild", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("wild", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("will", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("win", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("win", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wind", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wind", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("window", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("wine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wine", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("winter", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("winter", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wire", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wire", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wise", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("wish", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wish", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("with", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("woman", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular)
                .AddSuppletion(new Morpheme("women", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural)),
            new Morpheme("wonder", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wonder", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("word", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("word", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("work", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("work", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("world", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("worry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("worry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("worst", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("worst", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("worst", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //???
            new Morpheme("write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("wrong", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("wrong", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("wrong", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("wrong", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            
            new Morpheme("year", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("yellow", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            //??? yes je aj exclamation
            new Morpheme("yes", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("yet", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("you", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Second | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("young", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("young", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("your", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("yours", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Second | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            //??? zero
             new Morpheme("zoo", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
             new Morpheme("zoom", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("zoom", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            
            // Non-lexemes.
            new Morpheme(".", GrammarAttributes.PunctuationMark.Period),
            new Morpheme(",", GrammarAttributes.PunctuationMark.Comma),



            #region Inflectional suffixes

            // Noun plural
            new Morpheme("s", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular,
                    CanBindRule = EnglishWordRules.IsNounInBaseForm(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("s") | RuleMaker.EndsWithStr("x") | RuleMaker.EndsWithStr("z") | RuleMaker.EndsWithStr("sh") | RuleMaker.EndsWithStr("ch"), Trans.Append("e"))
                            ,
                            Trans.Append("s")
                        ),
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form,
                    CanBindRule = EnglishWordRules.IsVerbInBaseForm(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("s", "x", "z", "sh", "ch"), Trans.Append("e"))
                            ,
                            Trans.Append("s")
                        ),
                }
            },

            // Verb past-tense form
            new Morpheme("ed", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb.Form,
                    CanBindRule = EnglishWordRules.IsVerbInBaseForm(),
                    TransformValue = Trans
                        .Block(
                            // If it ends with short vowel and consonant then double the last consonant. e.g. putting.
                            Trans.If(EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel, Phoneme.Consonant), EnglishWordTrans.DoubleLastLetter())
                                // If it ends with 'e' then drop the 'e' e.g. joking.
                                .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                )
                            ,
                            Trans.Append("ing")
                        ),
                }
            },

            // Adjective comparative
            new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective.Comparative,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical,
                    CanBindRule = EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            // If it ends with short vowel and consonant then double the last consonant. e.g. putting.
                            Trans.If(EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel, Phoneme.Consonant), EnglishWordTrans.DoubleLastLetter())
                                // If it ends with 'e' then drop the 'e' e.g. joking.
                                .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                )
                            ,
                            Trans.Append("er")
                        ),
                }
            },

            // Adjective superlative
            new Morpheme("est", GrammarAttributes.Morpheme.Bound.Suffix.Inflectional)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective.Superlative,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical,
                    CanBindRule = EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            // If it ends with short vowel and consonant then double the last consonant. e.g. putting.
                            Trans.If(EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel, Phoneme.Consonant), EnglishWordTrans.DoubleLastLetter())
                                // If it ends with 'e' then drop the 'e' e.g. joking.
                                .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                )
                            ,
                            Trans.Append("est")
                        ),
                }
            },

            #endregion


            #region Suffixes creating nouns

            new Morpheme("age", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("age")
                        ),
                }
            },

            new Morpheme("al", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            .Else(Trans.If(RuleMaker.EndsWithStr("y") & EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            )
                            ,
                            Trans.Append("al")
                        ),
                }
            },

            new Morpheme("ance", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective() & EnglishWordRules.WordEndsWithStr("ant"),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("ant") | RuleMaker.EndsWithStr("ate"), Trans.DropFromEnd(0, 3))
                            .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                )
                            )
                            ,
                            Trans.Append("ance")
                        ),
                }
            },

            new Morpheme("dom", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("d"), Trans.DropFromEnd(0, 1))
                            .Else(Trans.If(RuleMaker.EndsWithStr("ee"), Trans.NothingToDo<string>())
                                .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                )
                            )
                            ,
                            Trans.Append("dom")
                        ),
                }
            },

            new Morpheme("ee", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ee")
                        ),
                }
            },

            new Morpheme("ence", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective() & EnglishWordRules.WordEndsWithStr("ent"),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("ent"), Trans.DropFromEnd(0, 3))
                            .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("ur"), Trans.Append("r"))
                                )
                            )
                            ,
                            Trans.Append("ence")
                        ),
                }
            },

            new Morpheme("er", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("er")
                        ),
                }
            },


            new Morpheme("ery", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            .Else(Trans.If(RuleMaker.EndsWithStr("b"), Trans.Append("b"))
                            )
                            ,
                            Trans.Append("ery")
                        ),
                }
            },

            new Morpheme("ful", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("ty", "cy", "dy", "ly"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ful")
                        ),
                }
            },

            new Morpheme("hood", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans.Append("hood"),
                }
            },



            new Morpheme("ption", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() & EnglishWordRules.WordEndsWithOneOfStr("pt", "scribe", "ceive", "sume"),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("pt"), Trans.DropFromEnd(0, 1))
                            .Else(Trans.If(RuleMaker.EndsWithStr("scribe"), Trans.DropFromEnd(0, 2))
                                .Else(Trans.If(RuleMaker.EndsWithStr("ceive"), Trans.DropFromEnd(0, 3))
                                    .Else(Trans.If(RuleMaker.EndsWithStr("sume"), Trans.DropFromEnd(0, 1))
                                    )
                                )
                            )
                            ,
                            Trans.Append("ption")
                        ),
                }
            },
            new Morpheme("tion", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() & (EnglishWordRules.WordEndsWithOneOfStr("ct", "ete", "ute", "it", "ite", "tain", "ose", "vene", "vent", "rt") | EnglishWordRules.WordEndsWithOneOfStr("intend", "contend")),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("ct"), Trans.DropFromEnd(0, 1)) 
                            .Else(Trans.If(RuleMaker.EndsWithStr("ete"), Trans.DropFromEnd(0, 2))
                                .Else(Trans.If(RuleMaker.EndsWithStr("ute"), Trans.DropFromEnd(0, 1))
                                    .Else(Trans.If(RuleMaker.EndsWithStr("it"), Trans.DropFromEnd(0, 1))
                                        .Else(Trans.If(RuleMaker.EndsWithStr("ite"), Trans.DropFromEnd(0, 2))
                                            .Else(Trans.If(RuleMaker.EndsWithStr("tain"), Trans.DropFromEnd(0, 3), Trans.Append("e"))
                                                .Else(Trans.If(RuleMaker.EndsWithStr("ose"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                                    .Else(Trans.If(RuleMaker.EndsWithStr("olve"), Trans.DropFromEnd(0, 2), Trans.Append("u"))
                                                        .Else(Trans.If(RuleMaker.EndsWithStr("vene") | RuleMaker.EndsWithStr("vent"), Trans.DropFromEnd(0, 1))
                                                            // Note: end - 2 exception words.
                                                            .Else(Trans.If(RuleMaker.EndsWithStr("end") | RuleMaker.EndsWithStr("rt"), Trans.DropFromEnd(0, 1))
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            ,
                            Trans.Append("tion")
                        ),
                }
            },
            new Morpheme("sion", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() & EnglishWordRules.WordEndsWithOneOfStr("de", "ise", "use", "pel", "mit", "cede", "ss", "end", "cline", "vert", "erse", "ur") & !EnglishWordRules.WordEndsWithOneOfStr("intend", "contend"),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("de"), Trans.DropFromEnd(0, 2))
                            .Else(Trans.If(RuleMaker.EndsWithStr("ise") | RuleMaker.EndsWithStr("use"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("pel"), Trans.DropFromEnd(0, 2), Trans.Append("ul"))
                                    .Else(Trans.If(RuleMaker.EndsWithStr("mit"), Trans.DropFromEnd(0, 1), Trans.Append("ss"))
                                        .Else(Trans.If(RuleMaker.EndsWithStr("cede"), Trans.DropFromEnd(0, 2), Trans.Append("ss"))
                                            .Else(Trans.If(RuleMaker.EndsWithStr("ss"), Trans.DropFromEnd(0, 1))
                                                .Else(Trans.If(RuleMaker.EndsWithStr("end") | RuleMaker.EndsWithStr("and"), Trans.DropFromEnd(0, 1))
                                                    .Else(Trans.If(RuleMaker.EndsWithStr("cline"), Trans.DropFromEnd(0, 3), Trans.Append("n"))
                                                        .Else(Trans.If(RuleMaker.EndsWithStr("vert"), Trans.DropFromEnd(0, 1))
                                                            .Else(Trans.If(RuleMaker.EndsWithStr("erse") | RuleMaker.EndsWithStr("erge"), Trans.DropFromEnd(0, 2))
                                                                .Else(Trans.If(RuleMaker.EndsWithStr("ur"), Trans.NothingToDo<string>())
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            ,
                            Trans.Append("sion")
                        ),
                }
            },


            new Morpheme("ication", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() & EnglishWordRules.WordEndsWithStr("fy"),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ication")
                        ),
                }
            },

            new Morpheme("ism", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("ze"), Trans.DropFromEnd(0, 2))
                            .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            )
                            ,
                            Trans.Append("ism")
                        ),
                }
            },

            new Morpheme("ist", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("ize"), Trans.DropFromEnd(0, 3))
                            .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                )
                            )
                            ,
                            Trans.Append("ist")
                        ),
                }
            },

            new Morpheme("ity", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("le"), Trans.DropFromEnd(0, 2), Trans.Append("il"))
                            .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("ine") | RuleMaker.EndsWithStr("ure"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                    .Else(Trans.If(RuleMaker.EndsWithStr("ous"), Trans.DropFromEnd(0, 3))
                                    )
                                )
                            )
                            ,
                            Trans.Append("ity")
                        ),
                }
            },

            new Morpheme("ment", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e") & EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Consonant, Phoneme.Vowel), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ment")
                        ),
                }
            },

            new Morpheme("ness", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ness")
                        ),
                }
            },

            new Morpheme("ology", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAnything(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("e", "a"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("al"), Trans.DropFromEnd(0, 2))
                                )
                            ,
                            Trans.Append("ology")
                        ),
                }
            },

            new Morpheme("or", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("or")
                        ),
                }
            },

            new Morpheme("ship", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans.Append("ship"),
                }
            },

            #endregion

            #region Suffixes creating verbs

            new Morpheme("ate", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun() | EnglishWordRules.IsBoundRoot(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ate")
                        ),
                }
            },

            new Morpheme("en", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("en")
                        ),
                }
            },

            new Morpheme("ify", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("e", "y"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ify")
                        ),
                }
            },

            new Morpheme("ize", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("e", "y"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ize")
                        ),
                }
            },


            #endregion

            #region Suffixes creating adjectives

            new Morpheme("able", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            // Note: seems that "der", "cer" do not double the last letter.
                            Trans.If(RuleMaker.EndsWithOneOfStr("ce", "ge", "der", "cer"), Trans.NothingToDo<string>())
                                .Else(Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                    .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                        .Else(Trans.If(EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel, Phoneme.Consonant), EnglishWordTrans.DoubleLastLetter())
                                        )
                                    )
                                )
                            ,
                            Trans.Append("able")
                        ),
                }
            },

            new Morpheme("al", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("e", "a"), Trans.DropFromEnd(0, 1))
                                .Else(Trans.If(RuleMaker.EndsWithStr("al"), Trans.DropFromEnd(0, 2))
                                )
                            ,
                            Trans.Append("al")
                        ),
                }
            },

            new Morpheme("en", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("en")
                        ),
                }
            },

            new Morpheme("ful", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("ty", "cy", "dy", "ly"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ful")
                        ),
                }
            },

            // Note: variation of the suffix al. But there is no clear rule when to use which.
            new Morpheme("ial", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("e", "a"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ial")
                        ),
                }
            },

            new Morpheme("ible", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            // Note: seems that "der", "cer" do not double the last letter.
                            Trans.If(RuleMaker.EndsWithOneOfStr("ce", "ge", "der", "cer", "limitable"), Trans.NothingToDo<string>())
                                .Else(Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                    .Else(Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                                        .Else(Trans.If(RuleMaker.EndsWithStr("mit"), Trans.DropFromEnd(0, 1), Trans.Append("ss"))
                                            .Else(Trans.If(RuleMaker.EndsWithStr("nd"), Trans.DropFromEnd(0, 1), Trans.Append("s"))
                                                .Else(Trans.If(RuleMaker.EndsWithStr("de"), Trans.DropFromEnd(0, 2), Trans.Append("s"))
                                                    .Else(Trans.If(RuleMaker.EndsWithStr("ive"), Trans.DropFromEnd(0, 3), Trans.Append("pt"))
                                                        .Else(Trans.If(EnglishWordRules.EndsWithPhonemes(Phoneme.Consonant, Phoneme.Vowel, Phoneme.Consonant), EnglishWordTrans.DoubleLastLetter())
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ,
                            Trans.Append("ible")
                        ),
                }
            },

            new Morpheme("ic", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ic")
                        ),
                }
            },

            new Morpheme("ical", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("y", "e"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ical")
                        ),
                }
            },

            new Morpheme("ive", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(),
                    TransformValue = Trans.Append("ive"),
                }
            },

            new Morpheme("ish", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans.Append("ish"),
                }
            },

            new Morpheme("less", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans.Append("less"),
                }
            },

            new Morpheme("ly", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans.Append("ly"),
                }
            },

            new Morpheme("ous", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans.Append("ous"),
                }
            },

            // Note: variation of the suffix al. But there is no clear rule when to use which.
            new Morpheme("ual", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithOneOfStr("e", "a"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ual")
                        ),
                }
            },

            new Morpheme("y", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("y")
                        ),
                }
            },

            #endregion

            #region Suffixes creating adverbs

            new Morpheme("ly", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adverb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            .Else(Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                                .Else(Trans.If(RuleMaker.EndsWithStr("ic"), Trans.Append("al"))
                                )
                            )
                            ,
                            Trans.Append("ly")
                        ),
                }
            },

            new Morpheme("wise", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adverb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(),
                    TransformValue = Trans.Append("wise"),
                }
            },

            new Morpheme("ward", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adverb,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free,
                    CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            // Note: the word fore drops 'e' -> forward.
                            Trans.If(RuleMaker.EndsWithStr("fore"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("ward")
                        ),
                }
            },

            #endregion

            
            // Prefixes
            new Morpheme("a", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsLexical() & EnglishWordRules.WordBeginsWithPhonemes(Phoneme.Vowel), TransformValue = Trans.Prepend("a"), } },
            new Morpheme("ab", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsLexical(), TransformValue = Trans.Prepend("ab"), } },
            new Morpheme("an", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsLexical() & EnglishWordRules.WordBeginsWithPhonemes(Phoneme.Consonant), TransformValue = Trans.Prepend("an"), } },
            new Morpheme("ante", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("ante"), } },
            new Morpheme("anti", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("anti"), } },
            new Morpheme("auto", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("auto"), } },

            new Morpheme("bene", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("bene"), } },
            new Morpheme("bi", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("bi"), } },

            new Morpheme("co", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("co"), } },
            new Morpheme("con", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("con"), } },
            new Morpheme("de", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("de"), } },
            new Morpheme("dis", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("dis"), } },

            new Morpheme("en", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("en"), } },
            new Morpheme("ex", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("ex"), } },
            new Morpheme("extra", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("extra"), } },

            new Morpheme("fore", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("fore"), } },

            new Morpheme("hetero", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("hetero"), } },
            new Morpheme("homo", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("homo"), } },
            new Morpheme("homeo", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("homeo"), } },
            new Morpheme("hyper", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("hyper"), } },
           
            new Morpheme("il", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & EnglishWordRules.WordBeginsWithStr("l"), TransformValue = Trans.Prepend("il"), } },
            new Morpheme("im", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & (EnglishWordRules.WordBeginsWithStr("b") | EnglishWordRules.WordBeginsWithStr("m") | EnglishWordRules.WordBeginsWithStr("p")), TransformValue = Trans.Prepend("im"), } },
            new Morpheme("in", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & !EnglishWordRules.WordBeginsWithStr("l") & !EnglishWordRules.WordBeginsWithStr("b") & !EnglishWordRules.WordBeginsWithStr("m") & !EnglishWordRules.WordBeginsWithStr("p") & !EnglishWordRules.WordBeginsWithStr("r"), TransformValue = Trans.Prepend("in"), } },
            new Morpheme("infra", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("infra"), } },
            new Morpheme("ir", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & EnglishWordRules.WordBeginsWithStr("r"), TransformValue = Trans.Prepend("ir"), } },
            new Morpheme("inter", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("inter"), } },
            new Morpheme("intra", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("intra"), } },

            new Morpheme("macro", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("macro"), } },
            new Morpheme("mal", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("mal"), } },
            new Morpheme("mega", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("mega"), } },
            new Morpheme("micro", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("micro"), } },
            new Morpheme("mid", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("mid"), } },
            new Morpheme("mis", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("mis"), } },
            new Morpheme("mono", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("mono"), } },
            new Morpheme("non", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("non"), } },

            new Morpheme("omni", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("omni"), } },
            new Morpheme("out", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("out"), } },
            new Morpheme("over", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("over"), } },

            new Morpheme("para", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("para"), } },
            new Morpheme("post", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("post"), } },
            new Morpheme("pre", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("pre"), } },
            new Morpheme("pro", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("pro"), } },

            new Morpheme("re", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("re"), } },

            new Morpheme("semi", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("semi"), } },
            new Morpheme("sub", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("sub"), } },
            new Morpheme("super", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("super"), } },

            new Morpheme("tele", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("tele"), } },
            new Morpheme("thermo", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("thermo"), } },
            new Morpheme("trans", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsLexical(), TransformValue = Trans.Prepend("trans"), } },
            new Morpheme("tri", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("tri"), } },

            new Morpheme("ultra", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("ultra"), } },
            new Morpheme("un", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("un"), } },
            new Morpheme("under", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("under"), } },
            new Morpheme("uni", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("uni"), } },
            new Morpheme("up", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("up"), } },
        };
    }
}
