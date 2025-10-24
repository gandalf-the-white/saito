"""
Configuration settings for MCP Flight Search.
"""

import os
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# Load API Key
SERP_API_KEY = os.getenv("SERP_API_KEY")

# Default server settings
DEFAULT_PORT = 3001
DEFAULT_CONNECTION_TYPE = "http"  # alternative: "stdio"
