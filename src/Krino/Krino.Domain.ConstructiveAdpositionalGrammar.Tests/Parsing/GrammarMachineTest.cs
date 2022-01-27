using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class GrammarMachineTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void GetTexts_I_read_the_book()
        {
            var i = new Word(new AdTree(new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun), new Pattern()));
            var read = new Word(new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent ), new Pattern()));
            var the = new Word(new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner), new Pattern()));
            var book = new Word(new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), new Pattern()));


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_I_give_him_a_book()
        {
            var i = new Word(new AdTree(new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun), new Pattern()));
            var give = new Word(new AdTree(new Morpheme(myAttributesModel, "give", EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent), new Pattern()));
            var him = new Word(new AdTree(new Morpheme(myAttributesModel, "him", EnglishAttributes.O.Lexeme.Pronoun), new Pattern()));
            var a = new Word(new AdTree(new Morpheme(myAttributesModel, "a", EnglishAttributes.A.Lexeme.Determiner), new Pattern()));
            var book = new Word(new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), new Pattern()));


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(give);
            grammar.Add(him);
            grammar.Add(a);
            grammar.Add(book);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }

        [Test]
        public void GetTexts_I_read_the_book_in_the_room()
        {
            var i = new Word(new AdTree(new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun), new Pattern()));
            var read = new Word(new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent), new Pattern()));
            var the = new Word(new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner), new Pattern()));
            var book = new Word(new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), new Pattern()));
            var in_ = new Word(new AdTree(new Morpheme(myAttributesModel, "in", EnglishAttributes.E.Lexeme.Preposition), new Pattern()));
            var room = new Word(new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), new Pattern()));


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);
            var kk = grammar.DebugView;
            grammar.Add(in_);
            grammar.Add(the);
            grammar.Add(room);

            var texts = grammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
        }
    }
}
