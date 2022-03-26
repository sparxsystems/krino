using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using NUnit.Framework;
using System.Linq;
using System.Threading;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class GrammarMachineTest
    {
        private GrammarMachine myGrammar;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            var english = new EnglishMachine().Machine;
            myGrammar = new GrammarMachine(english);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [Test]
        public void GetTexts_DirectObject()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(the);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
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

            myGrammar.Add(i);
            myGrammar.Add(give);
            myGrammar.Add(him);
            myGrammar.Add(a);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
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


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(the);
            myGrammar.Add(book);
            myGrammar.Add(in_);
            myGrammar.Add(the);
            myGrammar.Add(room);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(2, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_SubjectComplement()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(the);
            myGrammar.Add(book);
            myGrammar.Add(is_);
            myGrammar.Add(green);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_NounPhraseSubject()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word("flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(the);
            myGrammar.Add(green);
            myGrammar.Add(book);
            myGrammar.Add(flies);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_NounPhrase_With_AdjectivalPrepositionalPhrase()
        {
            var a = new Word("a", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var small = new Word("small", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var number = new Word("number", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var of = new Word("of", GrammarAttributes.Morpheme.Free.Functional.Preposition);
            var people = new Word("people", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word("flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(a);
            myGrammar.Add(small);
            myGrammar.Add(number);
            myGrammar.Add(of);
            myGrammar.Add(people);
            myGrammar.Add(flies);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_NounPhrase_With_AdjectivalClause()
        {
            var world = new Word("world", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var as_ = new Word("as", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);
            var few = new Word("few", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var people = new Word("people", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var says = new Word("says", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var ends = new Word("ends", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            Trace.StartProfiler();
            
            myGrammar.Add(world);

            Trace.StopProfiler();

            myGrammar.Add(as_);
            myGrammar.Add(few);
            myGrammar.Add(people);
            myGrammar.Add(says);
            myGrammar.Add(ends);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_ConcatNouns()
        {
            var pen = new Word("pen", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var and = new Word("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word("fly", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(pen);
            myGrammar.Add(and);
            myGrammar.Add(book);
            myGrammar.Add(flies);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
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


            _ = myGrammar.DebugView;

            myGrammar.Add(the);
            myGrammar.Add(green);
            myGrammar.Add(and);
            myGrammar.Add(blue);
            myGrammar.Add(book);
            myGrammar.Add(flies);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }



        [Test]
        public void GetTexts_InfinitiveAsAdjectiveComplement()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.FirstPerson);
            var tired = new Word("tired", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var to = new Word("to", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(am);
            myGrammar.Add(tired);
            myGrammar.Add(to);
            myGrammar.Add(read);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(2, texts.Count);

            myGrammar.Reset();
        }


        [Test]
        public void GetTexts_PresentSimple()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }


        [Test]
        public void GetTexts_PresentSimple_Negation()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var do_ = new Word("do", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(do_);
            myGrammar.Add(not);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }


        [Test]
        public void GetTexts_PresentContionuous()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(am);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PresentContionuous_Negation()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word("am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(am);
            myGrammar.Add(not);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PresentPerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PresentPerfect_Negation()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(not);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
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


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(been);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PresentContinuousPerfect_Negation()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var been = new Word("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(not);
            myGrammar.Add(been);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PastSimple()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var wrote = new Word("wrote", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(wrote);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PastContionuous()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var was = new Word("was", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(was);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PastPerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var had = new Word("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(had);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_PastContinuousPerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var had = new Word("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var been = new Word("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(had);
            myGrammar.Add(been);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }


        [Test]
        public void GetTexts_FutureSimple()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_FutureSimple_Negation()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var not = new Word("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(not);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_FutureContionuous()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var be = new Word("be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(be);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_FuturePerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(have);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_FutureContinuousPerfect()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var have = new Word("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var been = new Word("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word("reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(have);
            myGrammar.Add(been);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }



        [Test]
        public void GetTexts_NounAsAdjective()
        {
            var green = new Word("green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var race = new Word("race", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var car = new Word("car", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var fast = new Word("fast", GrammarAttributes.Morpheme.Free.Lexical.Adjective);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(green);
            myGrammar.Add(race);
            myGrammar.Add(car);
            myGrammar.Add(is_);
            myGrammar.Add(fast);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            myGrammar.Reset();
        }

        [Test]
        public void GetTexts_CompoundSentence()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var write = new Word("write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var and = new Word("and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            // First independent clause.
            myGrammar.Add(i);
            myGrammar.Add(read);

            myGrammar.Add(and);

            // Second independent clause.
            myGrammar.Add(i);
            myGrammar.Add(write);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
            Assert.AreEqual(1, texts[0].Sentences.Count);

            myGrammar.Reset();
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
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(want);
            myGrammar.Add(to);
            myGrammar.Add(see);

            // Second dependent clause.
            myGrammar.Add(what);
            myGrammar.Add(it);
            myGrammar.Add(is_);

            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(4, texts.Count);

            myGrammar.Reset();
        }


        [Test]
        public void GetTexts_TwoSentences()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var write = new Word("write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            // First sentence.
            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(punct);

            // Second sentence.
            myGrammar.Add(i);
            myGrammar.Add(write);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
            Assert.AreEqual(2, texts[0].Sentences.Count);

            myGrammar.Reset();
        }
    }
}
