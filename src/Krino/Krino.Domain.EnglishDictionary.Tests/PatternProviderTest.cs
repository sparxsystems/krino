using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Vertical.Utils.Diagnostic;
using NUnit.Framework;
using System.IO;
using System.Linq;

namespace Krino.Domain.EnglishDictionary.Tests
{
    [TestFixture]
    public class Tests
    {
        [Test]
        public void PluralNoun()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var adTreeCreator = new AdTreeCreator(dictionary);
            var results = adTreeCreator.Create("climates");

            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("O>O_s", results[0].Pattern.Name);
        }

        [Test]
        public void NounFromVerb()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var adTreeCreator = new AdTreeCreator(dictionary);
            var results = adTreeCreator.Create("walking");

            Assert.AreEqual(2, results.Count);
            Assert.AreEqual(1, results.Count(x => x.Pattern.Name == "I>O_ing"));
            Assert.AreEqual(1, results.Count(x => x.Pattern.Name == "I>I_ing"));
        }

        [Test]
        public void AdjectiveFromVerb()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var adTreeCreator = new AdTreeCreator(dictionary);
            var results = adTreeCreator.Create("prohibited");
            
            Assert.AreEqual(1, results.Count);
            Assert.AreEqual("I>A_ed", results[0].Pattern.Name);
        }
    }
}