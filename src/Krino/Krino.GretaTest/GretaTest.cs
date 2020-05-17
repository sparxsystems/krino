using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.GretaTest
{
    [TestFixture]
    public class GretaTest
    {
        [Test]
        public void Sentence_1_1()
        {
            ConstructiveDictionary dictionary = new ConstructiveDictionary(MorphemeProvider.Morphemes, PatternProvider.Patterns);
            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I have some good news and some bad news", 1);
        }
    }
}
