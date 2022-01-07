using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.EnglishGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class QuestionStateMachineFactoryTest
    {
        [Test]
        public void Create()
        {
            var factory = new QuestionStateMachineFactory();

            var machine = factory.Create();
            machine.Fire(EnglishAttributes.O.Lexeme.Pronoun);
            Assert.AreEqual("O", machine.State);

            machine = factory.Create();
            machine.Fire(EnglishAttributes.O.Lexeme.Pronoun.Interrogative);
            Assert.AreEqual("O", machine.State);

            //machine = factory.Create();
            //machine.Fire(EnglishAttributes.O.Lexeme);
            //Assert.AreEqual("O", machine.State);
        }
    }
}
