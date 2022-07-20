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

            new Morpheme("able", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("about", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("above", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("across", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("act", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("actor", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("active", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("activity", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("add", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent),
            new Morpheme("afraid", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("after", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("again", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("age", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("ago", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("agree", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("air", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("all", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("alone", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("along", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("already", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("always", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.FirstPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("among", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("amount", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("an", GrammarAttributes.Morpheme.Free.Functional.Determiner.IndefiniteArticle),
            new Morpheme("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("angry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("another", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //??? aky je to pronoun
            new Morpheme("another", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("answer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("any", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //???
            new Morpheme("anyone", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective),
            //???
            new Morpheme("anything", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective),
            //???
            new Morpheme("anytime", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("appear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("apple", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("are", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Plural.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Plural.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("area", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("arm", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("army", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("arrive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("around", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("art", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("ask", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("at", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("attack", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("aunt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("autumn", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("away", GrammarAttributes.Morpheme.Free.Lexical.Adverb),

            new Morpheme("baby", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("base", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("back", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bad", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("bag", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("ball", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bank", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("basket", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bath", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("bean", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            // ??? -ful
            new Morpheme("beatiful", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("beer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bed", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("behave", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("before", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //???
            new Morpheme("begin", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("behind", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("bell", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("below", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("besides", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("best", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("better", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("between", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("big", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("bird", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("birth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("birthday", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bite", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("black", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            //???
            new Morpheme("bleed", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("block", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("blood", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("blow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("blue", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("board", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("boat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("body", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("boil", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("bone", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("book", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("border", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("born", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("borrow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("both", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("both", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective),
            new Morpheme("bottle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? niekde pise ze nema plural niekde ze ma
            new Morpheme("bottom", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bowl", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("box", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("boy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("branch", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("brave", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("bread", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("break", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("breakfast", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("breathe", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("bridge", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bright", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("bring", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("brother", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("brown", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("brush", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("build", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("burn", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("business", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("bus", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("busy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("but", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            //???
            new Morpheme("buy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("by", GrammarAttributes.Morpheme.Free.Functional.Preposition),

            new Morpheme("cake", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("call", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("can", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal),
            new Morpheme("candle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cap", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("car", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("card", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("care", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("carry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("case", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("catch", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("central", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("century", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("certain", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("chair", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("change", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("chase", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("cheap", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cheese", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("chicken", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("child", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("chocolate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("choice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("choose", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("circle", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("city", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("class", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("clever", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("clean", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("clear", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("climb", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("clock", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cloth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cloud", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cloudy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("close", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("coffee", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("coat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("coin", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("cold", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("collect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("collect", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("colour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("comb", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("come", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("comfortable", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("common", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("compare", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("complete", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("computer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("condition", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("continue", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("control", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("cook", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("cool", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cooper", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("corn", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("corner", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("correct", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("cost", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("contain", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("count", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("country", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("course", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("cover", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("crash", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("crash", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("crease", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("crease", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("cross", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("cross", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("cry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("cry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("cup", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("cut", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),

            new Morpheme("dance", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("dance", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("danger", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dangerous", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("dark", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? plural ma tiez dark
            new Morpheme("dark", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("daugther", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("day", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dead", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dead", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("decide", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("decrease", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("decrease", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("deep", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("deep", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("deep", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("deer", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("depend", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("desk", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("destroy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("develop", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("die", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("die", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("different", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("dinner", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("direction", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dish", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("do", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("dog", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("door", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //???
            new Morpheme("double", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //???
            new Morpheme("down", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("draw", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("dream", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("dream", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("dress", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("dress", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("drink", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("drink", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("drive", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("drive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("drop", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("drop", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("dry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("dry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("duck", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("dust", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("dust", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("duty", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("destroy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("dedicate", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),

            new Morpheme("each", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("each", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("each", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("ear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("early", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("early", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("earn", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("earth", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("east", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("east", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("east", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("easy", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("easy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("eat", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("education", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("effect", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("effect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("egg", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? chceme aj cisla
            new Morpheme("eight", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("either", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("either", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("either", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("electric", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("electric", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("elephant", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("else", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("empty", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("empty", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("empty", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("enemy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("enjoy", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("enough", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("enough", GrammarAttributes.Morpheme.Free.Functional.Pronoun),
            new Morpheme("enough", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("enter", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("equal", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("equal", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("equal", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("entrance", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("entrance", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("escape", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("escape", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("even", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("even", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("even", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("evening", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("event", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("ever", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("every", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            //???
            new Morpheme("exact", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("exactly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("examination", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("example", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //??? ci je to Subordinating
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            //???
            new Morpheme("except", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("excite", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("exercise", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("exercise", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("expect", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("expensive", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("explain", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("extremely", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("eye", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),

            new Morpheme("face", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("face", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fact", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fail", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fail", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fall", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fall", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("false", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("false", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("family", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("famous", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("far", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("far", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("farm", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("farm", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("father", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fast", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fat", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fear", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fault", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("feed", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("feed", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("feel", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("feel", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("female", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fever", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? aky determiner a je to aj pronoun ale aky
            new Morpheme("few", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("flight", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("flight", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fill", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("film", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("film", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fild", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fild", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fine", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("finger", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? cudne sloveso
            new Morpheme("finger", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("finish", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("finish", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fire", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fire", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //??? toto je aj determiner a adverb
            new Morpheme("first", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("flag", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? cudne sloveso
            new Morpheme("flag", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("flat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("flat", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("fix", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("float", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("float", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("floor", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("floor", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("flour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("flour", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("flower", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("flower", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fly", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fly", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fly", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fold", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fold", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("food", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fool", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fool", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fool", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("foot", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? cudne sloveso
            new Morpheme("foot", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //??? je to aj preposition
            new Morpheme("for", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("force", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("force", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("foreign", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("forest", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("forget", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("forgive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fork", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? cudne sloveso
            new Morpheme("fork", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("form", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("form", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fox", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("free", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("free", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("freeze", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("freeze", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fresh", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("friend", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("friendly", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("from", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("front", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("front", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("front", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("fruit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("fruit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //??? ako to napisat full - fully
            new Morpheme("full", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("fully", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("fun", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("fun", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("furniture", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("further", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? cudne sloveso
            new Morpheme("further", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("future", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("future", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            new Morpheme("game", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("game", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("game", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("garden", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("garden", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("gate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("general", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("general", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? meni sa forma na men
            new Morpheme("gentleman", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("get", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("gift", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("gift", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("give", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("glad", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("glass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("glass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("go", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("goat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("god", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("gold", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("gold", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("grave", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("grave", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("great", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("great", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("green", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("grey", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("gray", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("ground", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("ground", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("grass", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("group", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("group", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            //???
            new Morpheme("grow", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
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
            new Morpheme("hammer", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("hand", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hand", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("happen", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("happy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hard", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hat", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hate", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hate", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("he", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("head", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("head", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("healthy", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("hear", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("heavy", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("heavy", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? toto je aj exclamation
            new Morpheme("hello", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? toto je aj exclamation
            new Morpheme("help", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("help", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("heart", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("heart", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("heaven", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("height", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hen", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("her", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("here", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfPlace),
            new Morpheme("hers", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("hide", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hide", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("high", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("high", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hill", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("him", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("his", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Masculine),
            new Morpheme("hit", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hit", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("hobby", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hold", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hold", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("hole", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hold", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("holiday", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("holiday", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("home", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("home", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("home", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("hope", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hope", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("horse", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("horse", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("hospital", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hot", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("hot", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("hotel", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("house", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("house", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("how", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            //??? aj cislovky chceme
            new Morpheme("hundred", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal),
            new Morpheme("hungry", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("hour", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hurry", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("hurry", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("husband", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("husband", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("hurt", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("hurt", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("hurt", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),

            new Morpheme("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("ice", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("ice", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("idea", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //??? je to aj noun
            new Morpheme("if", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("if", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("important", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //??? je to aj noun
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("in", GrammarAttributes.Morpheme.Free.Lexical.Adverb | GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("into", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            //???
            new Morpheme("introduce", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            //???
            new Morpheme("invent", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("iron", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("iron", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("iron", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),
            new Morpheme("invitation", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("invite", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson| GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("island", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("it", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Neuter),
            new Morpheme("its", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),

            new Morpheme("jelly", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("job", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("join", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            //???
            new Morpheme("join", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),



            new Morpheme("side", GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular),
            new Morpheme("sided", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            //???
            new Morpheme("side", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent),





            // Determiniers
            new Morpheme("a", GrammarAttributes.Morpheme.Free.Functional.Determiner.IndefiniteArticle),
            new Morpheme("some", GrammarAttributes.Morpheme.Free.Functional.Determiner),
            new Morpheme("the", GrammarAttributes.Morpheme.Free.Functional.Determiner.DefiniteArticle),
            new Morpheme("my", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("your", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("his", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),
            new Morpheme("their", GrammarAttributes.Morpheme.Free.Functional.Determiner.Possessive),


            // Conjunctions (onle 7 coordinating conjunctions)
            new Morpheme("nor", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("or", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("so", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("yet", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating),
            new Morpheme("after", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("although", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("as", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("since", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause),
            new Morpheme("that", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("though", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("whenever", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),
            new Morpheme("while", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating),


            // Verbs
            new Morpheme("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("was", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.FirstPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),
            new Morpheme("were", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Singular.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Plural.SecondPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past.Plural.ThirdPerson | GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary),

            new Morpheme("cycle", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("damage", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("drive", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent),
            new Morpheme("end", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
            new Morpheme("express", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base),
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
            new Morpheme("you", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Second | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("she", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("they", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Subjective  | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            new Morpheme("me", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("her", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Gender.Feminine),
            new Morpheme("us", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("them", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Objective | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            new Morpheme("maine", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.First | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular),
            new Morpheme("yours", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Second | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Singular | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),
            new Morpheme("theirs", GrammarAttributes.Morpheme.Free.Functional.Pronoun.Possessive | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Person.Third | GrammarAttributes.Morpheme.Free.Functional.Pronoun.Sememe.Number.Plural),

            
            // Adjectives
            new Morpheme("good", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("long", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("prohibited", GrammarAttributes.Morpheme.Free.Lexical.Adjective),
            new Morpheme("small", GrammarAttributes.Morpheme.Free.Lexical.Adjective),

            
            // Adverbs
            new Morpheme("actually", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("again", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("ahead", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("almost", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("also", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("certainly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("constantly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("currently", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("directly", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("daily", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Sememe.OfFrequency),
            new Morpheme("easily", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("else", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("especially", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("eventually", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("finally", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
            new Morpheme("generally", GrammarAttributes.Morpheme.Free.Lexical.Adverb),
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
            new Morpheme("orange", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("pink", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("purple", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("red", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("yellow", GrammarAttributes.Morpheme.Free.Lexical.Noun),
            new Morpheme("white", GrammarAttributes.Morpheme.Free.Lexical.Noun),

            // Prepositions
            
            new Morpheme("it", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("but", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("during", GrammarAttributes.Morpheme.Free.Functional.Preposition),
            new Morpheme("fro", GrammarAttributes.Morpheme.Free.Functional.Preposition),
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


            // Roots (Cannot stan alone. They comes from latin or greek.)
            new Morpheme("with", GrammarAttributes.Morpheme.Bound.Root),


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
                                Trans.ContinueWith(EnglishWordTrans.DoubleLastLetter(), Trans.Append("ing")))
                        // If it ends with 'e' then drop the 'e' e.g. joking.
                        .Else(Trans
                            .If(RuleMaker.EndsWithStr("e"),
                                Trans.ContinueWith(Trans.DropFromEnd(0, 1), Trans.Append("ing")))
                            .Else(Trans.Append("ing"))),
                }
            },

            #region Suffixes creating nouns

            new Morpheme("age", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    CanBindRule = EnglishWordRules.IsVerb(),
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb | GrammarAttributes.Morpheme.Free.Lexical.Adjective,
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

            new Morpheme("ance", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb | GrammarAttributes.Morpheme.Free.Lexical.Adjective,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    CanBindRule = EnglishWordRules.IsAdjective(),
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb | GrammarAttributes.Morpheme.Free.Lexical.Adjective,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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



            new Morpheme("ption", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb | GrammarAttributes.Morpheme.Free.Lexical.Adjective,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    CanBindRule = EnglishWordRules.IsVerb(),
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
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
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    CanBindRule = EnglishWordRules.IsAdjective(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("y"), Trans.DropFromEnd(0, 1), Trans.Append("i"))
                            ,
                            Trans.Append("ness")
                        ),
                }
            },

            new Morpheme("or", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    CanBindRule = EnglishWordRules.IsVerb(),
                    TransformValue = Trans
                        .Block(
                            Trans.If(RuleMaker.EndsWithStr("e"), Trans.DropFromEnd(0, 1))
                            ,
                            Trans.Append("or")
                        ),
                }
            },

            #endregion

            #region Suffixes creating adjectives

            new Morpheme("ive", GrammarAttributes.Morpheme.Bound.Suffix.Derivational)
            {
                Binding = new AffixBinding()
                {
                    AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                    AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical.Verb,
                    CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(),
                    TransformValue = Trans.Append("ive"),
                }
            },

            #endregion

            // Prefixes
            new Morpheme("a", GrammarAttributes.Morpheme.Bound.Prefix)
            {
                Binding = new AffixBinding()
                {
                    CanBindRule = EnglishWordRules.IsLexical() & EnglishWordRules.WordBeginsWithPhonemes(Phoneme.Vowel),
                    TransformValue = Trans.Prepend("a"),
                }
            },
            new Morpheme("an", GrammarAttributes.Morpheme.Bound.Prefix)
            {
                Binding = new AffixBinding()
                {
                    CanBindRule = EnglishWordRules.IsLexical() & EnglishWordRules.WordBeginsWithPhonemes(Phoneme.Consonant),
                    TransformValue = Trans.Prepend("an"),
                }
            },
            new Morpheme("ante", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("ante"), } },
            new Morpheme("anti", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("anti"), } },
            new Morpheme("auto", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("auto"), } },
            new Morpheme("co", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("co"), } },
            new Morpheme("con", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("con"), } },
            new Morpheme("de", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("de"), } },

            new Morpheme("dis", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("dis"), } },
            new Morpheme("en", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("en"), } },
            new Morpheme("ex", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() |EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("ex"), } },
            new Morpheme("extra", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("extra"), } },
            new Morpheme("hetero", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("hetero"), } },
            new Morpheme("homo", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("homo"), } },
            new Morpheme("homeo", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("homeo"), } },
            new Morpheme("hyper", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() | EnglishWordRules.IsNoun(), TransformValue = Trans.Prepend("hyper"), } },
           
            new Morpheme("il", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & EnglishWordRules.WordBeginsWithStr("l"), TransformValue = Trans.Prepend("il"), } },
            new Morpheme("im", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & (EnglishWordRules.WordBeginsWithStr("b") | EnglishWordRules.WordBeginsWithStr("m") | EnglishWordRules.WordBeginsWithStr("p")), TransformValue = Trans.Prepend("im"), } },
            new Morpheme("in", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & !EnglishWordRules.WordBeginsWithStr("l") & !EnglishWordRules.WordBeginsWithStr("b") & !EnglishWordRules.WordBeginsWithStr("m") & !EnglishWordRules.WordBeginsWithStr("p") & !EnglishWordRules.WordBeginsWithStr("r"), TransformValue = Trans.Prepend("in"), } },
            new Morpheme("ir", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsAdjective() & EnglishWordRules.WordBeginsWithStr("r"), TransformValue = Trans.Prepend("ir"), } },

            new Morpheme("inter", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("inter"), } },

            new Morpheme("macro", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("macro"), } },
            new Morpheme("micro", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("micro"), } },
            new Morpheme("mono", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("mono"), } },
            new Morpheme("non", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("non"), } },
            new Morpheme("post", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("post"), } },

            new Morpheme("pre", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb() | EnglishWordRules.IsAdjective(), TransformValue = Trans.Prepend("pre"), } },

            new Morpheme("re", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("re"), } },

            new Morpheme("sub", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("sub"), } },

            new Morpheme("tele", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("tele"), } },

            new Morpheme("trans", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("trans"), } },

            new Morpheme("un", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("un"), } },

            new Morpheme("uni", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsAdjective() | EnglishWordRules.IsAdverb(), TransformValue = Trans.Prepend("uni"), } },

            new Morpheme("up", GrammarAttributes.Morpheme.Bound.Prefix) { Binding = new AffixBinding() { CanBindRule = EnglishWordRules.IsNoun() | EnglishWordRules.IsVerb(), TransformValue = Trans.Prepend("up"), } },
        };
    }
}
