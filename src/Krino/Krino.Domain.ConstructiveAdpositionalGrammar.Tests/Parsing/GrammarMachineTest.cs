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
        public void GetTexts_I_read_the_book()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.O.Free.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.A.Free.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            var punct = new Word(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period);

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
        public void GetTexts_I_give_him_a_book()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.O.Free.Pronoun);
            var give = new Word("give", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Trivalent);
            var him = new Word("him", GrammarAttributes.Morpheme.O.Free.Pronoun);
            var a = new Word("a", GrammarAttributes.Morpheme.A.Free.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            var punct = new Word(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period);


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
        public void GetTexts_I_read_the_book_in_the_room()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.O.Free.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.A.Free.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            var in_ = new Word("in", GrammarAttributes.Morpheme.E.Free.Preposition);
            var room = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            var punct = new Word(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);
            grammar.Add(in_);
            grammar.Add(the);
            grammar.Add(room);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_The_book_is_green()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.A.Free.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.I.Free.Verb.Stative.Linking);
            var green = new Word("green", GrammarAttributes.Morpheme.A.Free.Adjective);
            var punct = new Word(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(the);
            grammar.Add(book);
            grammar.Add(is_);
            grammar.Add(green);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_The_green_book_is_on_the_table()
        {
            var the = new Word("the", GrammarAttributes.Morpheme.A.Free.Determiner);
            var green = new Word("green", GrammarAttributes.Morpheme.A.Free.Adjective);
            var book = new Word("book", GrammarAttributes.Morpheme.O.Free.Noun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.I.Free.Verb.Stative.Linking);
            var on = new Word("on", GrammarAttributes.Morpheme.E.Free.Preposition);
            var table = new Word("table", GrammarAttributes.Morpheme.O.Free.Noun);
            var punct = new Word(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(the);
            grammar.Add(green);
            grammar.Add(book);
            grammar.Add(is_);
            grammar.Add(on);
            grammar.Add(the);
            grammar.Add(table);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }


        [Test]
        public void GetTexts_I_am_tired_to_read()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.O.Free.Pronoun);
            var am = new Word("am", GrammarAttributes.Morpheme.I.Free.Verb.Stative.Linking);
            var tired = new Word("tired", GrammarAttributes.Morpheme.A.Free.Adjective);
            var to = new Word("to", GrammarAttributes.Morpheme.I.Free.Verb.InfinitiveMarker);
            var read = new Word("read", GrammarAttributes.Morpheme.I.Free.Verb.Valency.Trivalent);
            var punct = new Word(".", GrammarAttributes.Morpheme.U.Bound.PunctuationMark.Period);


            //Trace.StartProfiler();
            var english = new EnglishMachine().Machine;
            //Trace.StopProfiler();

            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(am);
            grammar.Add(tired);
            grammar.Add(to);
            grammar.Add(read);
            grammar.Add(punct);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }
    }
}
