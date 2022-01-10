using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.EnglishGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class EnglishStateMachineTest
    {
        [Test]
        public void Test()
        {
            var machine = new EnglishStateMachine();

            machine.Machine.Fire(EnglishAttributes.O.Lexeme.Noun);
            machine.Machine.Fire(EnglishAttributes.I.Lexeme.Verb);
        }
    }
}
