using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Parsing;
using NUnit.Framework;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class GrammarMachineTest
    {
        [Test]
        public void GetTexts_PresentSimple()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            var k = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_FutureSimple()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            var k = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(will);
            grammar.Add(read);
            grammar.Add(book);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }


        [Test]
        public void GetTexts_DirectObject()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            var k = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_IndirectObject()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var give = new Word("give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent);
            var him = new Word("him", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var a = new Word("a", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(give);
            grammar.Add(him);
            grammar.Add(a);
            grammar.Add(book);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_AdverbialAfterDirectObject()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var in_ = new Word("in", GrammarAttributes.Morpheme.Free.Functional.Preposition);
            var room = new Word("room", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);
            grammar.Add(in_);
            grammar.Add(the);
            grammar.Add(room);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(2, texts.Count);
        }

        [Test]
        public void GetTexts_SubjectComplement()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentThirdPersonSingular);
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(the);
            grammar.Add(book);
            grammar.Add(is_);
            grammar.Add(green);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_NounPhraseSubject()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word("flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(the);
            grammar.Add(green);
            grammar.Add(book);
            grammar.Add(flies);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_ConcatNouns()
        {
            var pen = new Word("pen", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var and = new Word("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word("fly", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(pen);
            grammar.Add(and);
            grammar.Add(book);
            grammar.Add(flies);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_ConcatAdjectives()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var and = new Word("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            var blue = new Word("blue", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word("flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(the);
            grammar.Add(green);
            grammar.Add(and);
            grammar.Add(blue);
            grammar.Add(book);
            grammar.Add(flies);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }



        [Test]
        public void GetTexts_InfinitiveAsAdjectiveComplement()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentFirstPersonSingular);
            var tired = new Word("tired", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var to = new Word("to", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            //Trace.StartProfiler();
            var english = new EnglishMachine().Machine;
            //Trace.StopProfiler();

            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(am);
            grammar.Add(tired);
            grammar.Add(to);
            grammar.Add(read);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(2, texts.Count);
        }

        [Test]
        public void GetTexts_PresentContionuous()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            //Trace.StartProfiler();
            var english = new EnglishMachine().Machine;
            //Trace.StopProfiler();

            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(am);
            grammar.Add(reading);
            grammar.Add(book);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_PresentPerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            //Trace.StartProfiler();
            var english = new EnglishMachine().Machine;
            //Trace.StopProfiler();

            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(have);
            grammar.Add(read);
            grammar.Add(book);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_PresentContinuousPerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var been = new Word("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            //Trace.StartProfiler();
            var english = new EnglishMachine().Machine;
            //Trace.StopProfiler();

            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(have);
            grammar.Add(been);
            grammar.Add(reading);
            grammar.Add(book);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }


        [Test]
        public void GetTexts_NounAsAdjective()
        {
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var race = new Word("race", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var car = new Word("car", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentThirdPersonSingular);
            var fast = new Word("fast", GrammarAttributes.Morpheme.Free.Lexical.Adjective);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(green);
            grammar.Add(race);
            grammar.Add(car);
            grammar.Add(is_);
            grammar.Add(fast);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_CompoundSentence()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var write = new Word("write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var and = new Word("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            // First independent clause.
            grammar.Add(i);
            grammar.Add(read);

            grammar.Add(and);

            // Second independent clause.
            grammar.Add(i);
            grammar.Add(write);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
            Assert.AreEqual(1, texts[0].Sentences.Count);
        }

        // Note: complex sentences consists of one main clause and one or more dependent clauses
        [Test]
        public void GetTexts_ComplexSentence_DependentSentence_As_DirectObject()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var want = new Word("want", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var to = new Word("to", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            var see = new Word("see", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var what = new Word("what", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);
            var it = new Word("it", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentThirdPersonSingular);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            grammar.Add(i);
            grammar.Add(want);
            grammar.Add(to);
            grammar.Add(see);

            // Second dependent clause.
            grammar.Add(what);
            grammar.Add(it);
            grammar.Add(is_);

            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(4, texts.Count);
        }


        [Test]
        public void GetTexts_TwoSentences()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var write = new Word("write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            // First sentence.
            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(punct);

            // Second sentence.
            grammar.Add(i);
            grammar.Add(write);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
            Assert.AreEqual(2, texts[0].Sentences.Count);
        }
    }
}
