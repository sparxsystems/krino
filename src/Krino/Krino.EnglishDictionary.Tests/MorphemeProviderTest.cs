using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishGrammar.Morphology;
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
            var morphology = new EnglishMorphology();

            var word = new Word(morphology, MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "book"));
            var pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            Assert.IsTrue(pluralSuffix.Binding.CanBind(word));

            word = new Word(morphology, MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "tax"));
            pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            Assert.IsFalse(pluralSuffix.Binding.CanBind(word));

            word = new Word(morphology, MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "people"));
            pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));
            Assert.IsFalse(pluralSuffix.Binding.CanBind(word));
        }

        [Test]
        public void BindingTransformation_Suffix_Ing()
        {
            var ingSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "ing" && GrammarAttributes.Morpheme.Bound.Suffix.Inflectional.IsIn(x.Attributes));

            Assert.AreEqual("ending", ingSuffix.Binding.TransformValue("end"));
            Assert.AreEqual("putting", ingSuffix.Binding.TransformValue("put"));
            Assert.AreEqual("joking", ingSuffix.Binding.TransformValue("joke"));
        }

        [Test]
        public void BindingTransformation_Suffix_Ation()
        {
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "ation" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));

            Assert.AreEqual("exploration", suffix.Binding.TransformValue("explore"));
            Assert.AreEqual("flirtation", suffix.Binding.TransformValue("flirt"));
        }

        [Test]
        public void BindingTransformation_Suffix_Sion()
        {
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "sion" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));

            Assert.AreEqual("invasion", suffix.Binding.TransformValue("invade"));
        }

        [Test]
        public void BindingTransformation_Suffix_Tion()
        {
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "tion" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));

            Assert.AreEqual("resolution", suffix.Binding.TransformValue("resolve"));
        }
    }
}
