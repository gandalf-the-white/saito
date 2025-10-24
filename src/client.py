#!/usr/bin/env python3
"""
Ollama Client to access on MCP Flight Search.
"""

import asyncio
import os
import sys
from llama_index.tools.mcp import BasicMCPClient, McpToolSpec
from llama_index.llms.ollama import Ollama
from llama_index.core.agent.workflow import ReActAgent

from prompt_templates import FLIGHT_SEARCH_PROMPT

# Configuration variables
MCP_URL = os.environ.get("MCP_URL", "http://localhost:3001/sse")
MODEL_NAME = os.environ.get("LLM_MODEL", "llama3.2")
TEMPERATURE = float(os.environ.get("LLM_TEMPERATURE", "0.7"))


async def setup_agent():
    """Setup and return the flight assistant agent"""
    try:
        # Connect to MCP server
        print(f"Connecting to MCP server at {MCP_URL}")
        mcp_client = BasicMCPClient(MCP_URL)

        # Get tools list
        print("Fetching available tools...")

        tools = await asyncio.wait_for(
            McpToolSpec(client=mcp_client).to_tool_list_async(), timeout=10
        )
        print(f"Found {len(tools)}")

        # Initialise Ollama LLM
        print(f"Initialisation Ollama with model {MODEL_NAME}...")
        llm = Ollama(model=MODEL_NAME, temperature=TEMPERATURE)

        # Create agent with flight search prompt
        system_prompt = (
            FLIGHT_SEARCH_PROMPT.template.replace("{toos}", "")
            .replace("{tool_names}", "")
            .replace("{input}", "")
        )
        agent = ReActAgent(
            name="FlightAgent",
            llm=llm,
            tools=tools,
            description="Agent using MCP Flight search tools with natural language understanding",
            system_prompt=system_prompt,
            temperature=TEMPERATURE,
            verbose=False,
        )

        return agent
    except Exception as e:
        print(f"Error setting up agent: {str(e)}")
        raise


async def main():
    """Main function to run the flight search application"""
    print("\n✈️ Natural Language Flight Search Assistant ✈️")
    print("-" * 50)
    print("Ask me anything about flights using natural language!")
    print("Examples:")
    print("  • Find flights from Atlanta to New York tomorrow")
    print("  • I need a flight to Paris next week")
    print("\nType 'exit' or 'quit' to end the session.")
    print("-" * 50)

    print("Make sure the flight server is running with:")
    print("mcp-flight-search --connection_type http")

    try:
        # Set up the agent
        agent = await setup_agent()
        print("Ready to search flights!")

        # Start conversation loop
        while True:
            user_query = input("\n🔍 Your flight query: ")

            if user_query.lower() in ["exit", "quit", "q"]:
                print("\nThank you for using the Flight Search Assistant. Goodbye!")
                break

            if user_query.strip():
                print("Searching for flights...")
                try:
                    response = await agent.run(user_query)
                    print(f"\n{response}")
                except Exception as e:
                    print(f"Error processing query: {e}")

    except Exception as e:
        print(f"Error: {e}")
        print(f"Make sure the flight server is running at {MCP_URL}")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
