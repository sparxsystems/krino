using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Parsing;
using NUnit.Framework;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class StatePathExtTest
    {
        [Test]
        public void GetText()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            // Although the sentence is not completed the text structure needs to contain all available elements.
            grammar.Add(i);

            var texts = grammar.ActiveStates.Select(x => x.Path.GetText()).ToList();
            var text = texts.FirstOrDefault();
            Assert.AreEqual("i", text.Value);
        }

        [Test]
        public void GetLastSentence()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);

            var give = new Word("give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent);
            var him = new Word("him", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var a = new Word("a", GrammarAttributes.Morpheme.Free.Functional.Determiner);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            _ = grammar.DebugView;

            // First sentence.
            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);
            grammar.Add(punct);

            // Second sentence.
            grammar.Add(i);
            grammar.Add(give);
            grammar.Add(him);
            grammar.Add(a);
            grammar.Add(book);
            grammar.Add(punct);


            // Although the sentence is not completed the text structure needs to contain all available elements.
            grammar.Add(i);

            var texts = grammar.ActiveStates.Select(x => x.Path.GetText()).ToList();
            var text = texts.FirstOrDefault();
            Assert.AreEqual("i", text.Value);
        }
    }
}
