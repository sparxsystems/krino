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
        public void CanBind()
        {
            var morphology = new EnglishMorphology();

            // plural suffix -s
            var word = new Word(morphology, MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "book"));
            var pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && x.Binding != null && GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural.IsIn(x.Binding.AttributesToPick));
            Assert.IsTrue(pluralSuffix.Binding.CanBind(word));

            // plural suffix -s on a plural word.
            word = new Word(morphology, MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "person")
                .Suppletions.FirstOrDefault(x => x.Value == "people"));
            pluralSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && x.Binding != null && GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural.IsIn(x.Binding.AttributesToPick));
            Assert.IsFalse(pluralSuffix.Binding.CanBind(word));

            // third person suffix -s on a noun
            var thirdPersonSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && x.Binding != null && GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson.IsIn(x.Binding.AttributesToPick));
            Assert.IsFalse(thirdPersonSuffix.Binding.CanBind(word));
        }


        [Test]
        public void BindingRule_Suffix_Plural()
        {
            var morphology = new EnglishMorphology();

            var sSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "s" && x.Binding != null && GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Plural.IsIn(x.Binding.AttributesToPick));
            Assert.AreEqual("books", sSuffix.Binding.TransformValue("book"));
            Assert.AreEqual("hashes", sSuffix.Binding.TransformValue("hash"));
        }

        [Test]
        public void BindingTransformation_Suffix_Er_Derivational()
        {
            var ingSuffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "er" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));

            Assert.AreEqual("writer", ingSuffix.Binding.TransformValue("write"));
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
            Assert.AreEqual("dramatization", suffix.Binding.TransformValue("dramatize"));
            Assert.AreEqual("aplification", suffix.Binding.TransformValue("aplify"));
            Assert.AreEqual("proclaimation", suffix.Binding.TransformValue("proclaim"));
        }

        [Test]
        public void BindingTransformation_Suffix_Sion()
        {
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "sion" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));
            Assert.AreEqual("invasion", suffix.Binding.TransformValue("invade"));
            Assert.AreEqual("commission", suffix.Binding.TransformValue("commit"));
            Assert.AreEqual("recession", suffix.Binding.TransformValue("recede"));
            Assert.AreEqual("impression", suffix.Binding.TransformValue("impress"));
            Assert.AreEqual("suspension", suffix.Binding.TransformValue("suspend"));
            Assert.AreEqual("diversion", suffix.Binding.TransformValue("divert"));
            Assert.AreEqual("aversion", suffix.Binding.TransformValue("averse"));
        }

        [Test]
        public void BindingTransformation_Suffix_Tion()
        {
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "tion" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));
            Assert.AreEqual("resolution", suffix.Binding.TransformValue("resolve"));
            Assert.AreEqual("education", suffix.Binding.TransformValue("educate"));

            // -efy
            Assert.AreEqual("putrefaction", suffix.Binding.TransformValue("putrefy"));

            Assert.AreEqual("adoption", suffix.Binding.TransformValue("adopt"));
            Assert.AreEqual("description", suffix.Binding.TransformValue("describe"));
            Assert.AreEqual("perception", suffix.Binding.TransformValue("perceive"));
            Assert.AreEqual("assumption", suffix.Binding.TransformValue("assume"));
            Assert.AreEqual("evolution", suffix.Binding.TransformValue("evolve"));
            Assert.AreEqual("invention", suffix.Binding.TransformValue("invent"));
            Assert.AreEqual("assertion", suffix.Binding.TransformValue("assert"));
        }

        [Test]
        public void BindingTransformation_Suffix_Cian()
        {
            var suffix = MorphemeProvider.Morphemes.FirstOrDefault(x => x.Value == "cian" && GrammarAttributes.Morpheme.Bound.Suffix.Derivational.IsIn(x.Attributes));
            Assert.AreEqual("magician", suffix.Binding.TransformValue("magic"));
            Assert.AreEqual("dietician", suffix.Binding.TransformValue("diet"));
        }
    }
}
