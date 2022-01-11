using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.StateMachines;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachine
    {
        private MultiMachine<LinguisticState, BigInteger> myMachine;

        public GrammarMachine(MultiMachine<LinguisticState, BigInteger> grammarMachine)
        {
            myMachine = grammarMachine;
        }

        public IEnumerable<Tree<StateRecord<LinguisticState, BigInteger>>> CurrentState => myMachine.ActiveStateRecords;

        public void Add(BigInteger morpheme)
        {
            myMachine.Fire(morpheme);
        }

        public void Reset() => myMachine.Reset();
    }
}
