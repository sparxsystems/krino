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
        public void GetTexts()
        {
            var i = new Word(new AdTree(new Morpheme(myAttributesModel, "i", EnglishAttributes.O.Lexeme.Pronoun), new Pattern()));
            var read = new Word(new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme.Verb), new Pattern()));
            var the = new Word(new AdTree(new Morpheme(myAttributesModel, "the", EnglishAttributes.A.Lexeme.Determiner), new Pattern()));
            var book = new Word(new AdTree(new Morpheme(myAttributesModel, "book", EnglishAttributes.O.Lexeme.Noun), new Pattern()));


            var english = new EnglishMachine().Machine;
            var grammar = new GrammarMachine(english);

            grammar.Add(i);
            grammar.Add(read);
            grammar.Add(the);
            grammar.Add(book);

            var text = grammar.GetTexts().ToList();
        }
    }
}
