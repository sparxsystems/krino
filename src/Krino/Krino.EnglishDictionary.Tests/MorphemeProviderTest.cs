using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Moq;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Krino.EnglishDictionary.Tests
{
    [TestFixture]
    public class MorphemeProviderTest
    {
        [Test]
        public void BindingRule_Suffix_Plural()
        {
            var word = new Word(MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "book"));
            var pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            Assert.IsTrue(pluralSuffix.Binding.Rule.Evaluate(word));

            word = new Word(MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "tax"));
            pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            Assert.IsFalse(pluralSuffix.Binding.Rule.Evaluate(word));

            word = new Word(MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "people"));
            pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            Assert.IsFalse(pluralSuffix.Binding.Rule.Evaluate(word));
        }

        [Test]
        public void BindingTransformation_Suffix_Ing()
        {
            var ingSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "ing" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));

            Assert.AreEqual("ending", ingSuffix.Binding.TransformWord.Transform("end"));
            Assert.AreEqual("putting", ingSuffix.Binding.TransformWord.Transform("put"));
            Assert.AreEqual("joking", ingSuffix.Binding.TransformWord.Transform("joke"));
        }
    }
}
