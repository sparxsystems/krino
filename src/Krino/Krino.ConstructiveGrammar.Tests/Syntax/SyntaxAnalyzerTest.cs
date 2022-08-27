using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishGrammar.Morphology;
using Krino.EnglishGrammar.Syntax;
using Krino.Vertical.Utils.Diagnostic;
using NUnit.Framework;
using System.Linq;
using System.Numerics;
using System.Threading;

namespace Krino.ConstructiveGrammar.Tests.Syntax
{
    [TestFixture]
    public class SyntaxAnalyzerTest
    {
        private SyntaxReader myGrammar;
        private IMorphology myMorphology;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            var english = new EnglishMachine(true).Machine;
            myGrammar = new SyntaxReader(english);
            myMorphology = new EnglishMorphology();

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [TearDown]
        public void TearDown()
        {
            myGrammar.Reset();
        }



        [Test]
        public void GetTexts_DirectObject()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word(myMorphology, "the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(the);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_IndirectObject()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var give = new Word(myMorphology, "give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent);
            var him = new Word(myMorphology, "him", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var a = new Word(myMorphology, "a", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            myGrammar.Add(i);
            myGrammar.Add(give);
            myGrammar.Add(him);
            myGrammar.Add(a);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_AdverbialAfterDirectObject()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word(myMorphology, "the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var in_ = new Word(myMorphology, "in", GrammarAttributes.Morpheme.Free.Functional.Preposition);
            var room = new Word(myMorphology, "room", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


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
            Assert.AreEqual(4, texts.Count);
        }

        [Test]
        public void GetTexts_SubjectComplement()
        {
            var the = new Word(myMorphology, "the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var is_ = new Word(myMorphology, "is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var green = new Word(myMorphology, "green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(the);
            myGrammar.Add(book);
            myGrammar.Add(is_);
            myGrammar.Add(green);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_NounPhraseSubject()
        {
            var the = new Word(myMorphology, "the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var green = new Word(myMorphology, "green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word(myMorphology, "flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(the);
            myGrammar.Add(green);
            myGrammar.Add(book);
            myGrammar.Add(flies);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_NounPhrase_With_AdjectivalPrepositionalPhrase()
        {
            var a = new Word(myMorphology, "a", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var small = new Word(myMorphology, "small", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var number = new Word(myMorphology, "number", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var of = new Word(myMorphology, "of", GrammarAttributes.Morpheme.Free.Functional.Preposition);
            var people = new Word(myMorphology, "people", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word(myMorphology, "flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

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

            
        }

        [Test]
        public void GetTexts_NounPhrase_With_AdjectivalClause()
        {
            var world = new Word(myMorphology, "world", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var as_ = new Word(myMorphology, "as", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);
            var few = new Word(myMorphology, "few", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var people = new Word(myMorphology, "people", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var say = new Word(myMorphology, "say", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var ends = new Word(myMorphology, "ends", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(world);
            myGrammar.Add(as_);
            myGrammar.Add(few);
            myGrammar.Add(people);
            myGrammar.Add(say);
            myGrammar.Add(ends);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_ConcatNouns()
        {
            var pen = new Word(myMorphology, "pen", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var and = new Word(myMorphology, "and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word(myMorphology, "fly", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(pen);
            myGrammar.Add(and);
            myGrammar.Add(book);
            myGrammar.Add(flies);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_ConcatAdjectives()
        {
            var the = new Word(myMorphology, "the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var green = new Word(myMorphology, "green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var and = new Word(myMorphology, "and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            var blue = new Word(myMorphology, "blue", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var flies = new Word(myMorphology, "flies", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


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

            
        }



        [Test]
        public void GetTexts_InfinitiveAsAdjectiveComplement()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word(myMorphology, "am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.FirstPerson);
            var tired = new Word(myMorphology, "tired", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var to = new Word(myMorphology, "to", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(am);
            myGrammar.Add(tired);
            myGrammar.Add(to);
            myGrammar.Add(read);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(2, texts.Count);

            
        }


        [Test]
        public void GetTexts_PresentSimple()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }


        [Test]
        public void GetTexts_PresentSimple_Negation()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var do_ = new Word(myMorphology, "do", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word(myMorphology, "not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(do_);
            myGrammar.Add(not);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }


        [Test]
        public void GetTexts_PresentContionuous()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word(myMorphology, "am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(am);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PresentContionuous_Negation()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var am = new Word(myMorphology, "am", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word(myMorphology, "not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(am);
            myGrammar.Add(not);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PresentPerfect()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word(myMorphology, "have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PresentPerfect_Negation()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word(myMorphology, "have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word(myMorphology, "not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(not);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PresentContinuousPerfect()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word(myMorphology, "have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var been = new Word(myMorphology, "been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(have);
            myGrammar.Add(been);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PresentContinuousPerfect_Negation()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var have = new Word(myMorphology, "have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var not = new Word(myMorphology, "not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var been = new Word(myMorphology, "been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


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

            
        }

        [Test]
        public void GetTexts_PastSimple()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var wrote = new Word(myMorphology, "wrote", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(wrote);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PastContionuous()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var was = new Word(myMorphology, "was", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(was);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PastPerfect()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var had = new Word(myMorphology, "had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(had);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_PastContinuousPerfect()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var had = new Word(myMorphology, "had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var been = new Word(myMorphology, "been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(had);
            myGrammar.Add(been);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }


        [Test]
        public void GetTexts_FutureSimple()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word(myMorphology, "will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_FutureSimple_Negation()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word(myMorphology, "will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var not = new Word(myMorphology, "not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(not);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_FutureContionuous()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word(myMorphology, "will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var be = new Word(myMorphology, "be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(be);
            myGrammar.Add(reading);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_FuturePerfect()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word(myMorphology, "will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var have = new Word(myMorphology, "have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(i);
            myGrammar.Add(will);
            myGrammar.Add(have);
            myGrammar.Add(read);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_FutureContinuousPerfect()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var will = new Word(myMorphology, "will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal);
            var have = new Word(myMorphology, "have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var been = new Word(myMorphology, "been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);
            var reading = new Word(myMorphology, "reading", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentParticiple | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

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

            
        }



        [Test]
        public void GetTexts_NounAsAdjective()
        {
            var green = new Word(myMorphology, "green", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var race = new Word(myMorphology, "race", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var car = new Word(myMorphology, "car", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            var is_ = new Word(myMorphology, "is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var fast = new Word(myMorphology, "fast", GrammarAttributes.Morpheme.Free.Lexical.Adjective);

            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(green);
            myGrammar.Add(race);
            myGrammar.Add(car);
            myGrammar.Add(is_);
            myGrammar.Add(fast);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);

            
        }

        [Test]
        public void GetTexts_CompoundSentence()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var write = new Word(myMorphology, "write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var and = new Word(myMorphology, "and", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

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
            Assert.AreEqual((BigInteger)GrammarAttributes.Sentence.Compound, texts[0].Sentences[0].Attributes);
        }

        // Note: complex sentences consists of one main clause and one or more dependent clauses
        [Test]
        public void GetTexts_ComplexSentence_DependentClause_As_DirectObject()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var want = new Word(myMorphology, "want", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var to = new Word(myMorphology, "to", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            var see = new Word(myMorphology, "see", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var what = new Word(myMorphology, "what", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);
            var it = new Word(myMorphology, "it", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var is_ = new Word(myMorphology, "is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);

            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

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
            Assert.AreEqual(2, texts.Count);
        }


        [Test]
        public void GetTexts_TwoSentences()
        {
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var write = new Word(myMorphology, "write", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);

            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

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

            
        }
    }
}
