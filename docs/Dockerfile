# Multi-stage build for Docusaurus site
FROM node:18-alpine AS builder

ARG ANTITHESIS_SITE_URL=https://cardano-foundation.github.io/
ARG ANTITHESIS_SITE_BASE_URL=/antithesis

ENV ANTITHESIS_SITE_URL=$ANTITHESIS_SITE_URL
ENV ANTITHESIS_SITE_BASE_URL=$ANTITHESIS_SITE_BASE_URL

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy source code
COPY . .

# Build the site
RUN npm run build
