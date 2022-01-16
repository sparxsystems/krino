using System.Collections.Generic;

namespace Krino.Vertical.Utils.StateMachines
{
    public class StateTrace<TState, TTrigger>
    {
        private List<StateItem<TState, TTrigger>> myPath = new List<StateItem<TState, TTrigger>>();

        public StateTrace(IEnumerable<StateItem<TState, TTrigger>> state)
        {
            myPath.AddRange(state);
        }

        public StateItem<TState, TTrigger> CurrentState => Count > 0 ? myPath[Count - 1] : null;

        public int Count => myPath.Count;

        public IReadOnlyList<StateItem<TState, TTrigger>> Trace => myPath;
    }
}
